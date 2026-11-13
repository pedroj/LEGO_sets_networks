import Foundation

/// Port of `classify_part.R` (same rules, same labels), so categories and
/// subcategories stay identical to the Shiny app and to the network plots.
///
/// The R code uses PCRE (`perl = TRUE`); `NSRegularExpression` (ICU) accepts every
/// pattern used there, including look-aheads.
public final class PartClassifier {
    public struct Result: Equatable {
        public let category: String
        public let subcategory: String
    }

    private var cache: [String: NSRegularExpression] = [:]
    private let lock = NSLock()

    public init() {}

    private func regex(_ pattern: String) -> NSRegularExpression {
        lock.lock(); defer { lock.unlock() }
        if let r = cache[pattern] { return r }
        let r = try! NSRegularExpression(pattern: pattern)
        cache[pattern] = r
        return r
    }

    private func has(_ pattern: String, _ text: String) -> Bool {
        let r = regex(pattern)
        return r.firstMatch(in: text, range: NSRange(text.startIndex..., in: text)) != nil
    }

    private func squash(_ s: String) -> String {
        regex("\\s+").stringByReplacingMatches(in: s, range: NSRange(s.startIndex..., in: s), withTemplate: " ")
    }

    /// `classify_part(name, category)`; `category` is the Rebrickable part category.
    public func classify(name: String?, category: String?) -> Result {
        // R: paste(NA) gives "NA", so a missing value becomes "na" after tolower()
        var n = squash(name ?? "NA").lowercased()
        let ct = (category ?? "NA").lowercased()
        func h(_ p: String) -> Bool { has(p, n) }       // reads the current `n`, like R's lazy default
        func hc(_ p: String) -> Bool { has(p, ct) }
        func out(_ c: String, _ s: String) -> Result { Result(category: c, subcategory: s) }

        if h("^[~=_]") {
            n = regex("^[~=_|]+").stringByReplacingMatches(in: n, range: NSRange(n.startIndex..., in: n), withTemplate: "")
        }

        // Minifig and accessories
        if h("\\bminifig") || hc("^minifig") {
            if h("\\bhead\\b(?!gear|dress|band)") && !h("hair|hat|helmet|cap\\b|headgear|crown|hood") {
                return out("Minifig", "Minifig head")
            }
            if h("torso") { return out("Minifig", "Minifig torso (with arms)") }
            if h("\\bhand\\b") { return out("Minifig", "Minifig hand") }
            if h("hips|legs?\\b") { return out("Minifig", "Minifig hips and legs") }
            if h("hair|hat|helmet|\\bcap\\b|headgear|headdress|crown|hood|headwear") || hc("headwear") {
                return out("Minifig", "Minifig headgear or hair")
            }
            if h("neck|backpack|cape|armou?r|vest|airtanks") { return out("Minifig", "Minifig neckwear") }
            return out("Minifig", "Minifig accessory (tool; weapon)")
        }
        // Wheels
        if h("\\bty(re|res)\\b|\\btire") { return out("Special", "Tyre") }
        if h("(brick|plate).*wheel|wheels? holder") && !h("^bar\\b") { return out("Special", "Wheel holder") }
        if h("\\bwheel|\\brim\\b") { return out("Special", "Wheel rim") }
        // Technic
        if h("technic") || hc("technic") {
            if h("\\bbrick\\b") { return out("Technic", "Technic brick (studded beam)") }
            if h("\\bplate\\b") { return out("Technic", "Technic plate (with holes)") }
            if h("\\bpin\\b") && h("\\bbar\\b") { return out("Technic", "Technic pin with bar") }
            if h("\\bpin\\b") && h("ball") { return out("Technic", "Technic pin with ball") }
            if h("axle pin|pin.*axle|axle.*pin") { return out("Technic", "Technic pin with axle") }
            if h("\\bpin\\b") && !h("connector|joiner|hole") { return out("Technic", "Technic pin") }
            if h("\\bbush") { return out("Technic", "Technic bush") }
            if h("gear|worm|\\brack\\b|differential|turntable") { return out("Technic", "Technic gear") }
            if h("chain|tread|track") { return out("Technic", "Technic chain or tread link") }
            if h("connector|joiner|block|cross|angle|hub|steering|shock|link") {
                return out("Technic", "Technic connector (cross block; angle)")
            }
            if h("\\baxle\\b") { return out("Technic", "Technic axle") }
            if h("panel|fairing") { return out("Technic", "Technic panel or fairing") }
            if h("\\bbrick\\b") { return out("Technic", "Technic brick (studded beam)") }
            if h("\\bplate\\b") { return out("Technic", "Technic plate (with holes)") }
            if h("beam|liftarm") { return out("Technic", "Technic beam (liftarm)") }
            return out("Technic", "Technic connector (cross block; angle)")
        }
        // Special connection parts
        if h("hinge") { return out("Special", "Click hinge (plate or brick)") }
        if h("turntable") { return out("Special", "Turntable") }
        if h("socket|ball cup") { return out("Special", "Ball-joint plate (socket)") }
        if h("tow ball|ball joint|with ball|\\bball\\b") { return out("Special", "Ball-joint plate (ball)") }
        if h("bracket") { return out("Special", "Bracket (SNOT)") }
        if h("\\bclip") { return out("Special", "Clip plate or tile") }
        if h("handle|bar holder|with bar\\b") { return out("Special", "Bar holder or handle plate") }
        if h("\\bdish\\b|radar") { return out("Special", "Dish or radar") }
        if h("\\bflag") { return out("Special", "Flag") }
        if h("hose|flex|\\btube\\b|\\bstring") { return out("Special", "Hose or flex bar") }
        if h("plant|leaves|flower|stem|grass|\\btree|vine|foliage") { return out("Special", "Plant or foliage") }
        if h("^(bar|antenna)|\\bbar \\d|antenna|lightsaber|whip|\\brod\\b") { return out("Special", "Bar or antenna") }
        // Structural families
        if h("baseplate") { return out("Baseplate", "Baseplate") }
        if h("windscreen|windshield|canopy") { return out("Panel", "Windscreen") }
        if h("glass|pane\\b|shutter|door\\b(?! ?frame)|\\bgate\\b") { return out("Panel", "Window or door leaf") }
        if h("window|door ?frame|\\bframe\\b") { return out("Panel", "Window frame") }
        if h("\\bpanel\\b|\\bwall\\b|\\bfence\\b") { return out("Panel", "Panel") }
        if h("^tile|\\btile\\b") || hc("^tiles") {
            if h("hollow bar|with bar|open stud|hollow stud") { return out("Tile", "Tile with bar or open stud") }
            if h("round|circle|quarter") { return out("Tile", "Tile round") }
            if h("grille|slope|modified|curved|corner") { return out("Tile", "Tile modified (grille; slope tile)") }
            return out("Tile", "Tile basic")
        }
        if h("slope|curved|\\bcurve\\b") || hc("slopes") {
            if h("inverted") { return out("Slope", "Slope inverted") }
            if h("curved|curve") { return out("Slope", "Slope curved") }
            return out("Slope", "Slope standard")
        }
        if h("\\bcone\\b") { return out("Brick", "Cone") }
        if h("\\barch\\b") { return out("Brick", "Brick arch") }
        if h("\\bwedge\\b") && h("plate") { return out("Plate", "Plate wedge") }
        if h("\\bwedge\\b") { return out("Slope", "Slope standard") }
        if h("^plate|\\bplate\\b") || hc("^plates") {
            if h("pin hole|with hole|axle hole") && !h("round") { return out("Plate", "Plate with pin hole") }
            if h("jumper|with 1 stud|1 x 2 with groove|centre stud|center stud") { return out("Plate", "Plate jumper") }
            if h("round|circle") { return out("Plate", "Plate round") }
            return out("Plate", "Plate basic")
        }
        if h("^brick|\\bbrick\\b") || hc("^bricks") {
            if h("round|cylinder") { return out("Brick", "Brick round") }
            if h("with (pin|axle)|with technic") { return out("Brick", "Brick with pin or axle") }
            if h("studs? on (\\d+ )?(side|sides)|headlight|with side studs?|snot") { return out("Brick", "Brick modified SNOT") }
            return out("Brick", "Brick basic")
        }
        return out("Other", "Other")
    }
}
