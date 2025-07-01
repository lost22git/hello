#!/usr/bin/env swift

import Foundation

func hide_cursor() {
    print("\u{1B}[?25l", terminator: "")
}

func show_cursor() {
    print("\u{1B}[?25h", terminator: "")
}

typealias ProgressBarPrefixProvider = (UInt, UInt) -> String
typealias ProgressBarSuffixProvider = (UInt, UInt) -> String

enum ProgressBarPrefix {
    case fixed(prefix: String)
    case provided(provider: ProgressBarPrefixProvider)
}
enum ProgressBarSuffix {
    case fixed(suffix: String)
    case provided(provider: ProgressBarSuffixProvider)
}

enum ProgressBarState {
    case new 
    case showing
    case done
}

class ProgressBar {
    let total: UInt
    var current: UInt = 0 {
        didSet {
            current = min(total, current)
        }
    }
    private(set) var state: ProgressBarState = .new 
    
    let cell: String
    let prefix: ProgressBarPrefix
    let suffix: ProgressBarSuffix
    let refresh_duration: Duration

    init(total: UInt, 
         cell: String = "█",
         prefix: ProgressBarPrefix = .fixed(prefix: ""),
         suffix: ProgressBarSuffix = .fixed(suffix: ""),
         refresh_duration: Duration = .milliseconds(100)
    ) {
       self.total = total 
       self.cell = cell
       self.prefix = prefix
       self.suffix = suffix
       self.refresh_duration = refresh_duration
    }

    func show() async throws {
        guard self.state == .new else {
            return
        }
        
        defer {
            self.state = .done
            show_cursor()
            print()
        }
        self.state = .showing
        hide_cursor()
        
        while current < total {
            _show()
            try await Task.sleep(for: refresh_duration)
        }
        _show()
    }
 
    private func _show() {
        let cell = self.cell
        let total = self.total
        let current = self.current
        
        let prefix = switch self.prefix  {
            case .fixed(let v): v
            case .provided(let provider): provider(total, current)
        }
        let suffix = switch self.suffix  {
            case .fixed(let v): v
            case .provided(let provider): provider(total, current)
        }
        
        let term_width = 66 // TODO: get real term width 
        let total_cols: UInt = UInt(term_width - prefix.count - suffix.count)
        let current_cols = current * total_cols / total
        let rest_cols = total_cols - current_cols

        let green = "\u{1B}[33m"
        let white = "\u{1B}[37m"
        let reset = "\u{1B}[0m"

        print("\r", terminator: "")
        print(prefix, separator: "",terminator: "")
        print(green,String(repeating: cell, count: Int(current_cols)) ,reset, separator: "",terminator: "")
        print(white,String(repeating: cell, count: Int(rest_cols)),reset, separator: "",terminator: "")
        print(suffix, separator: "",terminator: "")
    }
}

// === Main ===

var progress_bar = ProgressBar(total: 100, 
                               prefix: .provided { total, current in
                                    if current == total { "Downloaded" } else { "Downloading" }
                               },
                               suffix: .provided { total, current in
                                   "[\(current)/\(total)]"
                               })

Task{
   try await progress_bar.show()
}

while progress_bar.state != .done  {
  try await Task.sleep(for: .seconds(1))
  progress_bar.current += 10
}

assert(progress_bar.current == progress_bar.total, "current != total")
assert(progress_bar.state == .done, "state != .done")

