package main

/* Tansit
 *
 * deb-s3 as-a-service.
 *
 * Note this is intentionally single-threaded. The point of this service is to
 * serialise usage of deb-s3 so that race conditions are avoided. However,
 * races only happen over one bucket, so this could be made more intelligent
 * and parallelise calls made to separate buckets.
 */

import (
	"crypto/sha256"
	"encoding/base64"
	"encoding/json"
	"fmt"
	zmq "github.com/pebbe/zmq4"
	"io"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"path"
	"strings"
)

const (
	MSGTYPE_PKGDATA = "pkgdata"
	MSGTYPE_COMMAND = "command"
)

const (
	CMD_UPLOAD = "upload"
)

type Request struct {
	Cmd               string            `json:"cmd"`
	CmdOpts           map[string]string `json:"cmd_opts"`
	CmdArgs           []string          `json:"cmd_args"`
	PackageFileName   string            `json:"package_file_name"`
	PackageFileSHA256 string            `json:"package_file_sha256"`
	packageFilePath   string
}

type Response struct {
	Success       bool   `json:"success"`
	Message       string `json:"message,omitempty"`
	CommandOutput string `json:"command_output,omitempty"`
}

var TempDir string
var requests map[string]Request

func requestsKey(identity []byte) string {
	return base64.URLEncoding.EncodeToString(identity)
}

func main() {
	requests = make(map[string]Request)

	sock, err := zmq.NewSocket(zmq.ROUTER)
	if err != nil {
		panic(err)
	}

	err = sock.Bind(os.Args[1])
	if err != nil {
		panic(err)
	}

	TempDir, err = ioutil.TempDir("", "tansit-packages")
	if err != nil {
		panic(err)
	}

	log.Println("Tansit started")
	log.Printf("Temp dir: %s\n", TempDir)

	for {
		msg, err := sock.RecvMessageBytes(0)
		if err != nil {
			log.Printf("error receiving message: %s", err.Error())
			continue
		}

		processMsg(msg, sock)
	}
}

func processMsg(msg [][]byte, sock *zmq.Socket) {
	identity := msg[0]
	reqKey := requestsKey(identity)

	reply := func(resp Response) {
		body, err := json.Marshal(resp)
		if err != nil {
			panic(err)
		}
		sock.SendMessage(identity, body)
	}

	replyWithDebS3 := func(req Request) {
		output, err := req.runDebS3()
		resp := Response{CommandOutput: string(output)}
		if err == nil {
			resp.Success = true
		} else {
			resp.Success = false
			resp.Message = err.Error()
		}
		reply(resp)
	}

	abort := func(msg string) {
		reply(Response{Success: false, Message: msg})
		delete(requests, reqKey)
	}

	if len(msg) != 3 {
		log.Printf("Malformed message from identity %x, must be 3 parts", identity)
		return
	}

	msgType := string(msg[1])
	body := msg[2]

	switch msgType {
	case MSGTYPE_COMMAND:
		var req Request
		err := json.Unmarshal(body, &req)
		if err != nil {
			reply(Response{Success: false, Message: err.Error()})
			return
		}

		if _, ok := requests[reqKey]; ok {
			reply(Response{Success: false, Message: "Request already submitted"})
			return
		}

		if req.Cmd == CMD_UPLOAD {
			req.PackageFileName = strings.Replace(req.PackageFileName, string(os.PathSeparator), "_", -1)
			req.packageFilePath = path.Join(TempDir, req.PackageFileName)

			requests[reqKey] = req
			reply(Response{Success: true, Message: "OK, send the package"})
		} else {
			replyWithDebS3(req)
		}

	case MSGTYPE_PKGDATA:
		var req Request
		var ok bool
		if req, ok = requests[reqKey]; !ok {
			return
		}

		if len(body) == 0 {
			defer req.clean()

			err := req.verifyPackageFile()
			if err != nil {
				abort(err.Error())
			}

			replyWithDebS3(req)
		} else {
			err := req.appendPackage(body)
			if err != nil {
				abort(err.Error())
			}
		}
	}
}

func (req Request) appendPackage(data []byte) (err error) {
	f, err := os.OpenFile(req.packageFilePath, os.O_WRONLY|os.O_APPEND|os.O_CREATE, 0640)
	if err != nil {
		return
	}
	defer f.Close()

	_, err = f.Write(data)
	if err != nil {
		return
	}

	return nil
}

func (req Request) verifyPackageFile() (err error) {
	f, err := os.OpenFile(req.packageFilePath, os.O_RDONLY, 0640)
	if err != nil {
		return err
	}
	defer f.Close()

	h := sha256.New()
	_, err = io.Copy(h, f)
	if err != nil {
		return err
	}

	if fmt.Sprintf("%x", h.Sum(nil)) == strings.ToLower(req.PackageFileSHA256) {
		return nil
	} else {
		return fmt.Errorf("SHA256 %#v for %s doesn't match", req.PackageFileSHA256, req.PackageFileName)
	}
}

func (req Request) runDebS3() (output []byte, err error) {
	args := make([]string, 1, 1+len(req.CmdArgs)+len(req.CmdOpts))
	args[0] = req.Cmd
	for k, v := range req.CmdOpts {
		args = append(args, fmt.Sprintf("--%s=%s", k, v))
	}

	if req.packageFilePath != "" {
		args = append(args, req.packageFilePath)
	} else {
		for _, v := range req.CmdArgs {
			args = append(args, v)
		}
	}

	output, err = exec.Command("deb-s3", args...).CombinedOutput()
	return
}

func (req Request) clean() {
	_ = os.Remove(req.packageFilePath)
}
