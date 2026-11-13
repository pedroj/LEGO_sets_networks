import Foundation
import Compression

/// Streaming gzip decompression (Apple's Compression framework handles the raw
/// deflate stream; the gzip header is skipped by hand). Memory use stays flat even
/// for Rebrickable's large inventory_parts table.
enum Gzip {
    static func forEachChunk(of url: URL, _ body: (UnsafeBufferPointer<UInt8>) throws -> Void) throws {
        let fh = try FileHandle(forReadingFrom: url)
        defer { try? fh.close() }
        guard let first = try fh.read(upToCount: 1 << 16), first.count > 18 else {
            throw DBError(message: "\(url.lastPathComponent) is empty or not a gzip file")
        }
        let b = [UInt8](first)
        guard b[0] == 0x1f, b[1] == 0x8b, b[2] == 8 else {
            throw DBError(message: "\(url.lastPathComponent) is not a gzip file")
        }
        let flg = b[3]
        var pos = 10
        if flg & 4 != 0 { pos += 2 + (Int(b[pos]) | Int(b[pos + 1]) << 8) }
        if flg & 8 != 0 { while pos < b.count && b[pos] != 0 { pos += 1 }; pos += 1 }
        if flg & 16 != 0 { while pos < b.count && b[pos] != 0 { pos += 1 }; pos += 1 }
        if flg & 2 != 0 { pos += 2 }
        guard pos < b.count else { throw DBError(message: "Unsupported gzip header") }

        let dstSize = 1 << 18
        let dst = UnsafeMutablePointer<UInt8>.allocate(capacity: dstSize)
        defer { dst.deallocate() }
        var stream = compression_stream(dst_ptr: dst, dst_size: 0, src_ptr: UnsafePointer(dst), src_size: 0, state: nil)
        guard compression_stream_init(&stream, COMPRESSION_STREAM_DECODE, COMPRESSION_ZLIB) == COMPRESSION_STATUS_OK else {
            throw DBError(message: "Cannot start decompression")
        }
        defer { compression_stream_destroy(&stream) }

        var finished = false
        func pump(_ input: UnsafeBufferPointer<UInt8>, flags: Int32) throws {
            stream.src_ptr = input.baseAddress ?? UnsafePointer(dst)
            stream.src_size = input.count
            while !finished {
                stream.dst_ptr = dst
                stream.dst_size = dstSize
                let st = compression_stream_process(&stream, flags)
                let n = dstSize - stream.dst_size
                if n > 0 { try body(UnsafeBufferPointer(start: dst, count: n)) }
                if st == COMPRESSION_STATUS_END { finished = true }
                else if st == COMPRESSION_STATUS_ERROR { throw DBError(message: "Corrupt gzip data in \(url.lastPathComponent)") }
                else if flags == 0 && stream.src_size == 0 && stream.dst_size != 0 { break }
                else if flags != 0 && n == 0 { break }
            }
        }

        let head = Array(b[pos...])
        try head.withUnsafeBufferPointer { try pump($0, flags: 0) }
        while !finished, let data = try fh.read(upToCount: 1 << 18), !data.isEmpty {
            try data.withUnsafeBytes { raw in
                try pump(raw.bindMemory(to: UInt8.self), flags: 0)
            }
        }
        if !finished {
            try pump(UnsafeBufferPointer(start: nil, count: 0), flags: Int32(COMPRESSION_STREAM_FINALIZE.rawValue))
        }
    }
}
