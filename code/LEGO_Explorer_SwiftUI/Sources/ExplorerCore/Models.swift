import Foundation

/// Image paths in the database are stored without Rebrickable's common prefix.
public enum RebrickableImage {
    public static let prefix = "https://cdn.rebrickable.com/media/"
    public static func url(_ stored: String?) -> URL? {
        guard let s = stored, !s.isEmpty else { return nil }
        return URL(string: s.hasPrefix("http") ? s : prefix + s)
    }
}

public struct SetRecord: Identifiable, Hashable, Sendable {
    public let id: Int
    public let setNum: String
    public let name: String
    public let year: Int?
    public let numParts: Int
    public let imageURL: URL?
    public let themeName: String
    public let themeGroup: String
    // lower-cased copies for instant filtering
    public let numLower: String
    public let nameLower: String

    public init(id: Int, setNum: String, name: String, year: Int?, numParts: Int, image: String?,
                themeName: String?, themeGroup: String?) {
        self.id = id; self.setNum = setNum; self.name = name; self.year = year; self.numParts = numParts
        self.imageURL = RebrickableImage.url(image)
        self.themeName = themeName ?? ""
        self.themeGroup = themeGroup ?? themeName ?? ""
        self.numLower = setNum.lowercased(); self.nameLower = name.lowercased()
    }
}

public struct PartRecord: Identifiable, Hashable, Sendable {
    public let id: Int
    public let partNum: String
    public let name: String
    public let category: String
    public let subcategory: String
    public let rbCategory: String
    public let imageURL: URL?
    public let nSets: Int
    public let nColors: Int
    public let totalQty: Int
    public let numLower: String
    public let nameLower: String

    public init(id: Int, partNum: String, name: String, category: String, subcategory: String,
                rbCategory: String?, image: String?, nSets: Int, nColors: Int, totalQty: Int) {
        self.id = id; self.partNum = partNum; self.name = name; self.category = category
        self.subcategory = subcategory; self.rbCategory = rbCategory ?? ""
        self.imageURL = RebrickableImage.url(image)
        self.nSets = nSets; self.nColors = nColors; self.totalQty = totalQty
        self.numLower = partNum.lowercased(); self.nameLower = name.lowercased()
    }
}

/// One line of a set's parts list (a set × part × colour row, or several merged colours).
public struct PartLine: Identifiable, Hashable, Sendable {
    public let id: Int
    public let partID: Int
    public let partNum: String
    public let partName: String
    public let rbCategory: String
    public let category: String
    public let subcategory: String
    public let colorName: String      // merged lines: comma-separated colour names
    public let rgb: String
    public let quantity: Int
    public let isSpare: Bool
    public let minifig: String
    public let imageURL: URL?
}

public struct ColorLine: Identifiable, Hashable, Sendable {
    public var id: Int { colorID }
    public let colorID: Int
    public let name: String
    public let rgb: String
    public let sets: Int
    public let pieces: Int
    public let imageURL: URL?
}

public struct PartSetLine: Identifiable, Hashable, Sendable {
    public var id: Int { setID }
    public let setID: Int
    public let setNum: String
    public let setName: String
    public let year: Int?
    public let themeName: String
    public let quantity: Int
    public let colours: String
    public let imageURL: URL?
}

public struct DataInfo: Sendable {
    public var built = ""
    public var source = ""
    public var nSets = 0
    public var nParts = 0
    public var nRows = 0
    public var minifigParts = false
    public init() {}
}
