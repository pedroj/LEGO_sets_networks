import Cocoa
import QuickLook

@_cdecl("GenerateThumbnailForURL")
public func GenerateThumbnailForURL(
    _ thisInterface: UnsafeMutableRawPointer?,
    _ thumbnail: QLThumbnailRequest,
    _ url: CFURL,
    _ contentTypeUTI: CFString,
    _ options: CFDictionary?,
    _ maxSize: CGSize
) -> Int32 {
    let fileURL = url as URL
    guard let info = IOFileReader.readIOFile(at: fileURL) else { return 1 }
    let image = info.thumbnail
    let imageSize = image.size
    let scale = min(maxSize.width / imageSize.width, maxSize.height / imageSize.height, 1.0)
    let drawSize = CGSize(width: imageSize.width * scale, height: imageSize.height * scale)

    guard let contextRef = QLThumbnailRequestCreateContext(thumbnail, drawSize, false, nil) else { return 1 }
    let context = contextRef.takeRetainedValue()

    NSGraphicsContext.saveGraphicsState()
    NSGraphicsContext.current = NSGraphicsContext(cgContext: context, flipped: false)
    image.draw(in: CGRect(origin: .zero, size: drawSize))
    NSGraphicsContext.restoreGraphicsState()

    QLThumbnailRequestFlushContext(thumbnail, context)
    return 0
}

@_cdecl("GeneratePreviewForURL")
public func GeneratePreviewForURL(
    _ thisInterface: UnsafeMutableRawPointer?,
    _ preview: QLPreviewRequest,
    _ url: CFURL,
    _ contentTypeUTI: CFString,
    _ options: CFDictionary?
) -> Int32 {
    let fileURL = url as URL
    guard let info = IOFileReader.readIOFile(at: fileURL) else { return 1 }
    let image = info.thumbnail
    let imageSize = image.size

    guard let contextRef = QLPreviewRequestCreateContext(preview, imageSize, false, nil) else { return 1 }
    let context = contextRef.takeRetainedValue()

    NSGraphicsContext.saveGraphicsState()
    NSGraphicsContext.current = NSGraphicsContext(cgContext: context, flipped: false)
    image.draw(in: CGRect(origin: .zero, size: imageSize))

    let labelHeight: CGFloat = 30
    let labelRect = CGRect(x: 0, y: 0, width: imageSize.width, height: labelHeight)
    NSColor(white: 0, alpha: 0.6).setFill()
    labelRect.fill()

    let label = "Studio \(info.version) — \(info.totalParts) parts"
    let attrs: [NSAttributedString.Key: Any] = [
        .foregroundColor: NSColor.white,
        .font: NSFont.systemFont(ofSize: 14, weight: .medium)
    ]
    (label as NSString).draw(at: CGPoint(x: 8, y: 8), withAttributes: attrs)

    NSGraphicsContext.restoreGraphicsState()

    QLPreviewRequestFlushContext(preview, context)
    return 0
}

@_cdecl("CancelThumbnailGeneration")
public func CancelThumbnailGeneration(
    _ thisInterface: UnsafeMutableRawPointer?,
    _ thumbnail: QLThumbnailRequest
) {}

@_cdecl("CancelPreviewGeneration")
public func CancelPreviewGeneration(
    _ thisInterface: UnsafeMutableRawPointer?,
    _ preview: QLPreviewRequest
) {}
