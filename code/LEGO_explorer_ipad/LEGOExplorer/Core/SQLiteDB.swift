import Foundation
import SQLite3

/// Minimal SQLite wrapper (no third-party dependencies).
struct DBError: Error, LocalizedError {
    let message: String
    var errorDescription: String? { message }
}

enum DBValue: ExpressibleByIntegerLiteral, ExpressibleByStringLiteral, ExpressibleByNilLiteral {
    case int(Int), text(String), null
    init(integerLiteral v: Int) { self = .int(v) }
    init(stringLiteral v: String) { self = .text(v) }
    init(nilLiteral: ()) { self = .null }
    init(_ v: Int?) { self = v.map { .int($0) } ?? .null }
    init(_ v: String?) { self = v.map { .text($0) } ?? .null }
}

private let SQLITE_TRANSIENT = unsafeBitCast(-1, to: sqlite3_destructor_type.self)

final class Stmt {
    fileprivate var stmt: OpaquePointer?
    private let db: OpaquePointer?

    fileprivate init(db: OpaquePointer?, sql: String) throws {
        self.db = db
        if sqlite3_prepare_v2(db, sql, -1, &stmt, nil) != SQLITE_OK {
            throw DBError(message: "SQL error: \(String(cString: sqlite3_errmsg(db))) in: \(sql.prefix(120))")
        }
    }
    deinit { sqlite3_finalize(stmt) }

    func bind(_ args: [DBValue]) {
        sqlite3_reset(stmt)
        sqlite3_clear_bindings(stmt)
        for (i, a) in args.enumerated() {
            let idx = Int32(i + 1)
            switch a {
            case .int(let v): sqlite3_bind_int64(stmt, idx, Int64(v))
            case .text(let s): sqlite3_bind_text(stmt, idx, s, -1, SQLITE_TRANSIENT)
            case .null: sqlite3_bind_null(stmt, idx)
            }
        }
    }
    /// Steps once; returns true while a row is available.
    func step() throws -> Bool {
        let rc = sqlite3_step(stmt)
        if rc == SQLITE_ROW { return true }
        if rc == SQLITE_DONE { return false }
        throw DBError(message: "SQL error: \(String(cString: sqlite3_errmsg(db)))")
    }
    func run(_ args: [DBValue] = []) throws {
        bind(args)
        while try step() {}
    }
    func int(_ i: Int) -> Int { Int(sqlite3_column_int64(stmt, Int32(i))) }
    func optInt(_ i: Int) -> Int? {
        sqlite3_column_type(stmt, Int32(i)) == SQLITE_NULL ? nil : int(i)
    }
    func text(_ i: Int) -> String { optText(i) ?? "" }
    func optText(_ i: Int) -> String? {
        guard let c = sqlite3_column_text(stmt, Int32(i)) else { return nil }
        return String(cString: c)
    }
}

final class SQLiteDB {
    private(set) var handle: OpaquePointer?

    init(path: String, readonly: Bool = false) throws {
        let flags = readonly ? SQLITE_OPEN_READONLY : (SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE)
        if sqlite3_open_v2(path, &handle, flags, nil) != SQLITE_OK {
            let msg = handle.map { String(cString: sqlite3_errmsg($0)) } ?? "cannot open"
            sqlite3_close(handle)
            throw DBError(message: "Cannot open database: \(msg)")
        }
    }
    deinit { sqlite3_close(handle) }

    func exec(_ sql: String) throws {
        var err: UnsafeMutablePointer<CChar>?
        if sqlite3_exec(handle, sql, nil, nil, &err) != SQLITE_OK {
            let m = err.map { String(cString: $0) } ?? "unknown"
            sqlite3_free(err)
            throw DBError(message: "SQL error: \(m) in: \(sql.prefix(120))")
        }
    }
    func prepare(_ sql: String) throws -> Stmt { try Stmt(db: handle, sql: sql) }

    func query<T>(_ sql: String, _ args: [DBValue] = [], _ map: (Stmt) throws -> T) throws -> [T] {
        let s = try prepare(sql)
        s.bind(args)
        var out: [T] = []
        while try s.step() { out.append(try map(s)) }
        return out
    }
    func scalarInt(_ sql: String, _ args: [DBValue] = []) throws -> Int {
        try query(sql, args) { $0.int(0) }.first ?? 0
    }
}
