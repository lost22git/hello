package hello

import (
	"log/slog"
	"os"
	"testing"
)

type User struct {
	Id   int    `json:"id"`
	Name string `json:"name"`
}

// func (u User) LogValue() slog.Value {
// 	return slog.GroupValue(
// 		slog.Attr{Key: "id", Value: slog.IntValue(u.Id)},
// 		slog.Attr{Key: "name", Value: slog.StringValue(u.Name)},
// 	)
// }

func TestLogSlog(t *testing.T) {
	// Default
	slog.SetLogLoggerLevel(slog.LevelDebug)
	logSomething()

	// Structure logging

	logLevel := &slog.LevelVar{}
	logLevel.Set(slog.LevelDebug)

	// Json
	jsonhandler := slog.NewJSONHandler(os.Stdout, &slog.HandlerOptions{AddSource: true, Level: logLevel})
	jsonlogger := slog.New(jsonhandler).With("handler", "Json")

	// Dynamic change default logger
	slog.SetDefault(jsonlogger)

	logSomething()

	// Text
	texthandler := slog.NewTextHandler(os.Stdout, &slog.HandlerOptions{AddSource: true, Level: logLevel})
	textlogger := slog.New(texthandler).With("handler", "Text")

	// Dynamic change default logger
	slog.SetDefault(textlogger)

	// Dynamic change log level
	logLevel.Set(slog.LevelError)

	logSomething()
}

func logSomething() {
	user := User{Id: 11, Name: "gogo"}
	slog.Debug("log a DEBUG msg", "user", user)
	slog.Info("log a INFO msg", "user", user)
	slog.Warn("log a WARN msg", "user", user)
	slog.Error("log a ERROR msg", "user", user)
}
