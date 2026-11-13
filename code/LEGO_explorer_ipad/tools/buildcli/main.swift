import Foundation
// Mac command-line twin of the app's "Update tables": CSV folder -> SQLite.
//   buildcli <csv_folder> <out.sqlite>
let a = CommandLine.arguments
guard a.count >= 3 else { print("usage: buildcli <csv folder> <out.sqlite>"); exit(2) }
let t0 = Date()
do {
    try DataBuilder.build(sourceDir: URL(fileURLWithPath: a[1]), output: URL(fileURLWithPath: a[2]),
                          sourceLabel: "CSV folder (buildcli)") { m, f in
        print(String(format: "%5.1f s  %3.0f%%  %@", Date().timeIntervalSince(t0), f * 100, m))
    }
} catch { print("FAILED: \(error.localizedDescription)"); exit(1) }
