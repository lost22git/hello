package main

import (
	"bufio"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log/slog"
	"mime/multipart"
	"net/http"
	"os"
	"path"
	"time"
)

// {"status":200,"success":true,"data":{"id":"JEE8NZv","deletehash":"vnr
// Marp2ZKyjOhC","account_id":null,"account_url":null,"ad_type":null,"ad
// _url":null,"title":null,"description":null,"name":"","type":"image/jp
// eg","width":474,"height":692,"size":144611,"views":0,"section":null,"
// vote":null,"bandwidth":0,"animated":false,"favorite":false,"in_galler
// y":false,"in_most_viral":false,"has_sound":false,"is_ad":false,"nsfw"
// :null,"link":"https://i.imgur.com/JEE8NZv.jpeg","tags":[],"datetime":
// 1750520472,"mp4":"","hls":""}}

type UnixTime struct {
	time.Time
}

func (ut UnixTime) MarshalJSON() ([]byte, error) {
	return fmt.Appendf(nil, "%d", ut.Unix()), nil
}

func (ut *UnixTime) UnmarshalJSON(bytes []byte) error {
	var timestamp int64
	err := json.Unmarshal(bytes, &timestamp)
	if err != nil {
		return err
	}
	ut.Time = time.Unix(timestamp, 0)
	return nil
}

type Result struct {
	Status  int        `json:"status"`
	Success bool       `json:"success"`
	Data    ResultData `json:"data"`
}

type ResultData struct {
	Id         string `json:"id"`
	Deletehash string `json:"deletehash"`

	Type     string   `json:"type"`
	Width    int      `json:"width"`
	Height   int      `json:"height"`
	Size     int      `json:"size"`
	Datetime UnixTime `json:"datetime"`
	Link     string   `json:"link"`
	Tags     []string `json:"tags"`
}

func writeFilePart(mw *multipart.Writer, fieldName string, filePath string, bufferSize int) error {
	filename := path.Base(filePath)

	w, err := mw.CreateFormFile(fieldName, filename)
	if err != nil {
		return err
	}

	f, err := os.Open(filePath)
	if err != nil {
		return err
	}
	defer f.Close()

	r := bufio.NewReaderSize(f, bufferSize)

	len, err := io.Copy(w, r)
	slog.Debug("IO copied", "length", len)
	if err != nil {
		return err
	}

	return nil
}

func upload(filePath string) (*Result, error) {
	const url = "https://api.imgur.com/3/image?client_id=546c25a59c58ad7"

	// create pipe for streamingly uploading
	pr, pw := io.Pipe()
	defer pr.Close()

	mw := multipart.NewWriter(pw)
	contentType := mw.FormDataContentType()

	go func() {
		defer pw.Close()
		defer mw.Close()
		const bufferSize = 1024 * 8
		err := writeFilePart(mw, "image", filePath, bufferSize)
		if err != nil {
			slog.Error("Error on writing file part", "err", err)
		}
	}()

	resp, err := http.Post(url, contentType, pr)
	if err != nil {
		return nil, errors.Join(
			errors.New("Error on http posting"),
			err,
		)
	}

	slog.Debug("Got response", "status", resp.Status)
	slog.Debug("Got response", "header", resp.Header)

	respBodyReader := resp.Body
	defer respBodyReader.Close()

	bytes, err := io.ReadAll(respBodyReader)
	if err != nil {
		return nil, errors.Join(
			errors.New("Error on reading response body"),
			err,
		)
	}

	slog.Debug("Got response", "body", fmt.Sprintf("%s\n", bytes))

	var result Result
	err = json.Unmarshal(bytes, &result)
	if err != nil {
		return nil, errors.Join(
			errors.New("Error on json unmarshalling to Result"),
			err,
		)
	}

	return &result, nil
}

func main() {
	slog.SetLogLoggerLevel(slog.LevelDebug)

	filePath := "./棋魂.jpg"

	result, err := upload(filePath)
	if err != nil {
		slog.Error("Error on uploading:", "err", err)
		return
	}

	slog.Info("Upload file successfully", "result", result)
}
