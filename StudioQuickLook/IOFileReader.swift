import Foundation
import AppKit
import Compression

struct IOFileInfo {
    let version: String
    let totalParts: Int
    let thumbnail: NSImage
}

enum IOFileReader {
    private static let password = "soho0909"

    static func readIOFile(at url: URL) -> IOFileInfo? {
        guard let archive = MiniZip(url: url, password: password) else { return nil }

        var version = ""
        var totalParts = 0

        if let infoData = archive.readEntry(named: ".info"),
           let json = try? JSONSerialization.jsonObject(with: infoData) as? [String: Any] {
            version = (json["version"] as? String)?.replacingOccurrences(of: "\\r", with: "") ?? ""
            totalParts = json["total_parts"] as? Int ?? 0
        }

        guard let thumbData = archive.readEntry(named: "thumbnail.png"),
              let image = NSImage(data: thumbData) else { return nil }

        return IOFileInfo(version: version, totalParts: totalParts, thumbnail: image)
    }
}

// Minimal ZIP reader supporting traditional PKZIP encryption
final class MiniZip {
    private let data: Data
    private let password: [UInt8]
    private var entries: [String: (offset: Int, compressedSize: Int, uncompressedSize: Int, method: UInt16, isEncrypted: Bool)] = [:]

    init?(url: URL, password: String) {
        guard let data = try? Data(contentsOf: url) else { return nil }
        self.data = data
        self.password = Array(password.utf8)
        parseEntries()
        if entries.isEmpty { return nil }
    }

    private func parseEntries() {
        var offset = 0
        while offset + 30 <= data.count {
            let sig = data.readUInt32(at: offset)
            guard sig == 0x04034b50 else { break }

            let flags = data.readUInt16(at: offset + 6)
            let method = data.readUInt16(at: offset + 8)
            let compressedSize = Int(data.readUInt32(at: offset + 18))
            let uncompressedSize = Int(data.readUInt32(at: offset + 22))
            let nameLen = Int(data.readUInt16(at: offset + 26))
            let extraLen = Int(data.readUInt16(at: offset + 28))
            let nameStart = offset + 30
            let name = String(data: data[nameStart..<nameStart+nameLen], encoding: .utf8) ?? ""
            let dataStart = nameStart + nameLen + extraLen
            let isEncrypted = (flags & 1) != 0

            entries[name] = (offset: dataStart, compressedSize: compressedSize,
                             uncompressedSize: uncompressedSize, method: method, isEncrypted: isEncrypted)

            offset = dataStart + compressedSize
        }
    }

    func readEntry(named name: String) -> Data? {
        guard let entry = entries[name] else { return nil }
        var compData = Data(data[entry.offset..<entry.offset+entry.compressedSize])

        if entry.isEncrypted {
            guard compData.count >= 12 else { return nil }
            var keys: (UInt32, UInt32, UInt32) = (0x12345678, 0x23456789, 0x34567890)
            for b in password {
                updateKeys(&keys, with: b)
            }
            for i in 0..<compData.count {
                let k = decryptByte(keys)
                compData[i] ^= k
                updateKeys(&keys, with: compData[i])
            }
            compData = compData.dropFirst(12)
        }

        if entry.method == 0 {
            return compData
        } else if entry.method == 8 {
            return decompressDeflate(compData, expectedSize: entry.uncompressedSize)
        }
        return nil
    }

    private func updateKeys(_ keys: inout (UInt32, UInt32, UInt32), with byte: UInt8) {
        keys.0 = crc32Update(keys.0, byte: byte)
        keys.1 = (keys.1 &+ (keys.0 & 0xFF)) &* 134775813 &+ 1
        keys.2 = crc32Update(keys.2, byte: UInt8((keys.1 >> 24) & 0xFF))
    }

    private func decryptByte(_ keys: (UInt32, UInt32, UInt32)) -> UInt8 {
        let temp = (keys.2 | 2) & 0xFFFF
        return UInt8(((temp &* (temp ^ 1)) >> 8) & 0xFF)
    }

    private func crc32Update(_ crc: UInt32, byte: UInt8) -> UInt32 {
        let table = MiniZip.crc32Table
        return table[Int((crc ^ UInt32(byte)) & 0xFF)] ^ (crc >> 8)
    }

    private func decompressDeflate(_ data: Data, expectedSize: Int) -> Data? {
        let capacity = max(expectedSize, data.count * 4)
        var decompressed = Data(count: capacity)
        let written = data.withUnsafeBytes { srcPtr -> Int in
            decompressed.withUnsafeMutableBytes { destPtr -> Int in
                guard let srcBase = srcPtr.baseAddress,
                      let destBase = destPtr.baseAddress else { return 0 }
                let result = compression_decode_buffer(
                    destBase.assumingMemoryBound(to: UInt8.self), capacity,
                    srcBase.assumingMemoryBound(to: UInt8.self), data.count,
                    nil, COMPRESSION_ZLIB)
                return result
            }
        }
        guard written > 0 else { return nil }
        decompressed.count = written
        return decompressed
    }

    private static let crc32Table: [UInt32] = {
        (0..<256).map { i -> UInt32 in
            var crc = UInt32(i)
            for _ in 0..<8 {
                crc = (crc & 1) != 0 ? (0xEDB88320 ^ (crc >> 1)) : (crc >> 1)
            }
            return crc
        }
    }()
}

private extension Data {
    func readUInt16(at offset: Int) -> UInt16 {
        self.subdata(in: offset..<offset+2).withUnsafeBytes { $0.load(as: UInt16.self).littleEndian }
    }
    func readUInt32(at offset: Int) -> UInt32 {
        self.subdata(in: offset..<offset+4).withUnsafeBytes { $0.load(as: UInt32.self).littleEndian }
    }
}
