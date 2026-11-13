import Foundation

/// Final database layout. Must match tools/make_sqlite.R.
enum Schema {
    static let tables = """
    CREATE TABLE sets(set_id INTEGER PRIMARY KEY, set_num TEXT NOT NULL COLLATE NOCASE, set_name TEXT, year INTEGER, theme_id INTEGER, num_parts INTEGER, set_img_url TEXT, theme_name TEXT, theme_group TEXT);
    CREATE TABLE parts(part_id INTEGER PRIMARY KEY, part_num TEXT NOT NULL COLLATE NOCASE, part_name TEXT, category TEXT, subcategory TEXT, part_category TEXT, part_cat_id INTEGER, part_material TEXT, part_img_url TEXT, n_sets INTEGER, n_colors INTEGER, total_qty INTEGER);
    CREATE TABLE colors(color_id INTEGER PRIMARY KEY, color_name TEXT, rgb TEXT, is_trans INTEGER);
    CREATE TABLE figs(fig_id INTEGER PRIMARY KEY, fig_num TEXT);
    CREATE TABLE set_parts(set_id INTEGER NOT NULL, part_id INTEGER NOT NULL, color_id INTEGER, quantity INTEGER, is_spare INTEGER, fig_id INTEGER);
    CREATE TABLE part_color_img(part_id INTEGER NOT NULL, color_id INTEGER NOT NULL, img_url TEXT, PRIMARY KEY(part_id, color_id)) WITHOUT ROWID;
    CREATE TABLE build_info(key TEXT PRIMARY KEY, value TEXT);
    """
    static let indexes = """
    CREATE INDEX sets_num ON sets(set_num);
    CREATE INDEX sets_theme ON sets(theme_group, theme_name);
    CREATE INDEX parts_num ON parts(part_num);
    CREATE INDEX set_parts_set ON set_parts(set_id);
    CREATE INDEX set_parts_part ON set_parts(part_id);
    """
}
