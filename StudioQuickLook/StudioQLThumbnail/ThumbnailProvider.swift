import QuickLookThumbnailing
import AppKit

class ThumbnailProvider: QLThumbnailProvider {
    override func provideThumbnail(for request: QLFileThumbnailRequest,
                                   _ handler: @escaping (QLThumbnailReply?, Error?) -> Void) {
        guard let info = IOFileReader.readIOFile(at: request.fileURL) else {
            handler(nil, NSError(domain: "StudioQL", code: 1))
            return
        }

        let image = info.thumbnail
        let imageSize = image.size
        let maxSize = request.maximumSize

        let scale = min(maxSize.width / imageSize.width, maxSize.height / imageSize.height)
        let drawSize = CGSize(width: imageSize.width * scale, height: imageSize.height * scale)

        let reply = QLThumbnailReply(contextSize: drawSize, drawing: { ctx -> Bool in
            NSGraphicsContext.saveGraphicsState()
            NSGraphicsContext.current = NSGraphicsContext(cgContext: ctx, flipped: false)
            image.draw(in: CGRect(origin: .zero, size: drawSize))
            NSGraphicsContext.restoreGraphicsState()
            return true
        })

        handler(reply, nil)
    }
}
