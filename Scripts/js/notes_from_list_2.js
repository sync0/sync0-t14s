<%*
    // Zettel structure vars // 
var zettel_delimiter = "---\n";
var zettel_type_line = "zettel_type: ";
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
var postfix_size = 3;//the size of string 
var alphabet = "abcdefghijkmnpqrstuvwxyz"; //from where to create
var postfix = "";

var props = {
    wordPrompt: {
        prompt: "Enter word to add to list",
        fallback: "None specified",
        type: "string"
    },
};

// // Set up projects//

var project = await tp.system.suggester(["deutsch", "portuguese", "french", "korean", "russian"], ["deutsch", "portuguese", "french", "korean", "russian"], false, "Choose language");

var words = [];
var word;

while (confirm("Add new word?")) {
    // Calculate the file name for current word //
    for ( var i=0; i < postfix_size; i++ )
        postfix += alphabet[Math.floor(Math.random() * alphabet.length)];
    let prefix = tp.date.now("YYMMDD");
    let file_title = prefix + await postfix;

    // Define values for the new Zettel //
    let title = await tp.system.prompt("Word?");
    let translation = await tp.system.prompt("Translation?");
    let translation_string = translation_line + translation + "\n";
    let tag_fiche = + await tp.system.suggester(["adjective", "adverb", "conjunction", "collocation", "noun", "verb", "lexis", "preposition"], ["adjective", "adverb", "conjunction", "collocation", "noun", "verb", "adjective", "lexis", "preposition"], false, "Fiche type?");
    let part_of_speech_string = part_of_speech_line + tag_fiche + "\n";
    let id_string = id_line + file_title + "\n";
    let zettel_type_string = tags_line + file_title + "\n";
    let aliases_string = aliases_line + file_title + "\n";
    let tags_string = tags_line + "[fiche/" + tag_fiche + ", project/" + project + ", language/" project "]\n";

    // Define content for the new Zettel //
    let file_contents = zettel_delimiter + id_string + created_string + aliases_string + translation_string + part_of_speech_string + tags_string + zettel_delimiter + header_symbol + title;

    // Create  the new Zettel //
    tp.file.create_new(file_contents, file_title, false, tp.file.folder());
}

%>
