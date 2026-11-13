import Foundation
import SQLite3

public struct SQLiteError: Error, LocalizedError {
    public let message: String
    public var errorDescription: String? { message }
}

/// Minimal wrapper around the SQLite3 C API (no third-party dependencies).
public final class SQLiteDB {
    public private(set) var handle: OpaquePointer?
    private static let transient = unsafeBitCast(-1, to: sqlite3_destructor_type.self)

    public init(path: String, readOnly: Bool) throws {
        let flags = readOnly ? SQLITE_OPEN_READONLY : (SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE)
        if sqlite3_open_v2(path, &handle, flags | SQLITE_OPEN_FULLMUTEX, nil) != SQLITE_OK {
            let msg = handle.map { String(cString: sqlite3_errmsg($0)) } ?? "cannot open \(path)"
            sqlite3_close(handle)
            throw SQLiteError(message: msg)
        }
    }

    deinit { close() }

    public func close() {
        if let h = handle { sqlite3_close_v2(h); handle = nil }
    }

    public var lastError: String { handle.map { String(cString: sqlite3_errmsg($0)) } ?? "closed" }

    public func exec(_ sql: String) throws {
        var err: UnsafeMutablePointer<CChar>?
        if sqlite3_exec(handle, sql, nil, nil, &err) != SQLITE_OK {
            let m = err.map { String(cString: $0) } ?? lastError
            sqlite3_free(err)
            throw SQLiteError(message: "\(m) — \(sql.prefix(120))")
        }
    }

    public enum Bind {
        case int(Int), text(String), null
    }

    public struct Row {
        let stmt: OpaquePointer
        public func int(_ i: Int32) -> Int { Int(sqlite3_column_int64(stmt, i)) }
        public func optInt(_ i: Int32) -> Int? {
            sqlite3_column_type(stmt, i) == SQLITE_NULL ? nil : Int(sqlite3_column_int64(stmt, i))
        }
        public func text(_ i: Int32) -> String {
            guard let c = sqlite3_column_text(stmt, i) else { return "" }
            return String(cString: c)
        }
        public func optText(_ i: Int32) -> String? {
            guard let c = sqlite3_column_text(stmt, i) else { return nil }
            return String(cString: c)
        }
    }

    public func prepare(_ sql: String) throws -> OpaquePointer {
        var stmt: OpaquePointer?
        if sqlite3_prepare_v2(handle, sql, -1, &stmt, nil) != SQLITE_OK {
            throw SQLiteError(message: "\(lastError) — \(sql.prefix(120))")
        }
        return stmt!
    }

    public func bind(_ stmt: OpaquePointer, _ binds: [Bind]) {
        for (i, b) in binds.enumerated() {
            let idx = Int32(i + 1)
            switch b {
            case .int(let v): sqlite3_bind_int64(stmt, idx, Int64(v))
            case .text(let s): sqlite3_bind_text(stmt, idx, s, -1, SQLiteDB.transient)
            case .null: sqlite3_bind_null(stmt, idx)
            }
        }
    }

    /// Runs a query and calls `row` for each result row.
    public func query(_ sql: String, _ binds: [Bind] = [], row: (Row) -> Void) throws {
        let stmt = try prepare(sql)
        defer { sqlite3_finalize(stmt) }
        bind(stmt, binds)
        while true {
            let rc = sqlite3_step(stmt)
            if rc == SQLITE_ROW { row(Row(stmt: stmt)) }
            else if rc == SQLITE_DONE { break }
            else { throw SQLiteError(message: lastError) }
        }
    }

    public func scalarInt(_ sql: String, _ binds: [Bind] = []) throws -> Int {
        var v = 0
        try query(sql, binds) { v = $0.int(0) }
        return v
    }
}
