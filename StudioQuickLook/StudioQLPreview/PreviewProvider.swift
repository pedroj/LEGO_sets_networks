import Cocoa
import Quartz

class PreviewProvider: QLPreviewProvider, QLPreviewingController {
    func providePreview(for request: QLFilePreviewRequest,
                        completionHandler handler: @escaping (QLPreviewReply?, Error?) -> Void) {
        guard let info = IOFileReader.readIOFile(at: request.fileURL) else {
            handler(nil, NSError(domain: "StudioQL", code: 1))
            return
        }

        let image = info.thumbnail
        let size = image.size

        let reply = QLPreviewReply(dataOfContentType: .png, contentSize: size) { reply in
            let bitmapRep = NSBitmapImageRep(
                bitmapDataPlanes: nil,
                pixelsWide: Int(size.width),
                pixelsHigh: Int(size.height),
                bitsPerSample: 8,
                samplesPerPixel: 4,
                hasAlpha: true,
                isPlanar: false,
                colorSpaceName: .deviceRGB,
                bytesPerRow: 0,
                bitsPerPixel: 0
            )!

            NSGraphicsContext.saveGraphicsState()
            NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: bitmapRep)
            image.draw(in: CGRect(origin: .zero, size: size))
            NSGraphicsContext.restoreGraphicsState()

            return bitmapRep.representation(using: .png, properties: [:]) ?? Data()
        }

        reply.title = "Studio \(info.version) — \(info.totalParts) parts"
        handler(reply, nil)
    }
}
