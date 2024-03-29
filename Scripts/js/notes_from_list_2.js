<%*
    // Zettel structure vars // 
var zettel_delimiter = "---\n";
var zettel_type_string = "zettel_type: fiche\n";
var title;
var header_symbol = "# ";
var id_line = "id: ";
var today_date = tp.date.now("YYYY-MM-DD");
var created_string = "created: " + today_date + "\n";
var aliases_line = "aliases: ";
var translation_line = "translation: ";
var part_of_speech_line = "part_of_speech: ";
var tags_line = "tags: ";

// Zettelkasten prefixer file name generator set up //
var postfix_size = 3; //the size of string 
var alphabet = "abcdefghijkmnpqrstuvwxyz"; //from where to create
var prefix = tp.date.now("YYMMDD");
var postfix = "";

// // Set up projects//

var project = await tp.system.suggester(["deutsch", "portuguese", "french", "korean", "russian"], ["deutsch", "portuguese", "french", "korean", "russian"], false, "Choose language");

while (confirm("Add new word?")) {
    // Calculate the file name for current word //
    for ( var i=0; i < postfix_size; i++ )
        postfix += alphabet[Math.floor(Math.random() * alphabet.length)];
    let file_title = prefix + await postfix;

    // Define values for the new Zettel //
    let title = await tp.system.prompt("Word?");
    let translation = await tp.system.prompt("Translation?");
    let translation_string = translation_line + await translation + "\n";
    let tag_fiche = await tp.system.suggester(["adjective", "adverb", "conjunction", "collocation", "noun", "verb", "lexis", "preposition"], ["adjective", "adverb", "conjunction", "collocation", "noun", "verb", "adjective", "lexis", "preposition"], false, "Fiche type?");
    let part_of_speech_string = part_of_speech_line + await tag_fiche + "\n";
    let id_string = id_line + await file_title + "\n";
    let aliases_string = aliases_line + '"' + await title + '"' + "\n";
    let tags_string = tags_line + "[fiche/" + await tag_fiche + ", project/" + await project + ", language/" await project "]\n";

    // Define content for the new Zettel //
    let file_contents = zettel_delimiter + await id_string + zettel_type_string + created_string + await aliases_string + await translation_string + await part_of_speech_string + await tags_string + zettel_delimiter + header_symbol + await title;

    // Create  the new Zettel //
    tp.file.create_new(file_contents, file_title, false, tp.file.folder());
}

%>
