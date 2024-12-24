#!/usr/bin/env swift

func myFunction() {
    print("current function is:", #function)
    #warning("compliation warning")
}

myFunction()
