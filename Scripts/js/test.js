var props = {
    firstName: {
        prompt: "Enter first name",
        fallback: "None specified",
        type: "string"
    },
    lastName: {
        prompt: "Enter last name",
        fallback: "None specified",
        type: "string"
    },
    phoneNumber: {
        prompt: "Enter telephone number",
        fallback: "None specified",
        type: "string"
    },
    email: {
        prompt: "Enter e-mail address",
        fallback: "None specified",
        type: "string"
    },
    prompter: function (obj) {
        return prompt(obj.prompt,obj.fallback);
    }
};

var contacts = [];
var contact;
var addContacts = function() {
    while (confirm("Add new contact?")) {
        contact = {};
        for (var x in props) {
            if (typeof props[x] !== 'function') {
                contact[x] = props.prompter(props[x]);
            }
        }
        contacts[contacts.length] = contact;
    }
    console.log("Bye.");
};

addContacts();
console.log(contacts);






// Set random Zettel name//
var file_title = "";
var postfix = "";
var postfix_size = 2;//the size of string 
// let alphabet = "abcdefghijkmnpqrstuvwxyz"; //from where to create //
var alphabet = "0123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnpqrstuvwxyz"; //from where to create
var prefix = tp.date.now("YYDDD");
// let prefix = tp.date.now("YYMMDD");//

function createTitle() {
    for ( var i=0; i < postfix_size; i++ )
        postfix += alphabet[Math.floor(Math.random() * alphabet.length)];
    file_title = prefix + postfix;
    if (tp.file.exists(file_title)) {
        createTitle();
    } else {
        tp.file.rename(file_title);
    }
}

createTitle();


function linkRelated(bibkey) {
    let x = "[[" + bibkey + "]]"
    return x
}



// function extractYamlProperties(file) {
//   const content = app.vault.metadataCache(file);
//   const match = content.match(/^---[\s\S]*?^---/m);
//   if (match) {
//     try {
//       return YAML.parse(match[0]);
//     } catch (error) {
//       console.error("Error parsing YAML properties:", error);
//     }
//   }
//   return {};
// }

var subfolderName = "references"; // Specify the name of the stable subfolder

var suggestions = (await tp.system.suggester(
    (item) => {
        const author = await tp.frontmatter["author"] ;
        const origdate = await tp.frontmatter["origdate"] ;
        const date = await tp.frontmatter["date"] ;
        const title = await tp.frontmatter["title"] ;
        const subtitle = await tp.frontmatter["subtitle"] ;
        return `${author} ${origdate} ${date} ${title}${subtitle}`;
  },
  app.vault.getMarkdownFiles().filter((file) => {
    const filePath = file.path;
    const subfolderIndex = filePath.indexOf(subfolderName);
    return subfolderIndex !== -1 && filePath.substring(subfolderIndex).split("/").length === 2;
  }),
  false,
  "Select source of annotation/citation"
));


var selectedFile = await suggestions.file;
var annotation = await selectedFile.basename;





var subfolderName = "references"; // Specify the name of the stable subfolder

var suggestions = (await tp.system.suggester(
    (item) => {
        const author = tp.frontmatter["author"] ;
        const origdate = tp.frontmatter["origdate"] ;
        const date = tp.frontmatter["date"] ;
        const title = tp.frontmatter["title"] ;
        const subtitle = tp.frontmatter["subtitle"] ;
        return `${author} ${origdate} ${date} ${title}${subtitle}`;
  },
  app.vault.getMarkdownFiles().filter((file) => {
    const filePath = file.path;
    const subfolderIndex = filePath.indexOf(subfolderName);
    return subfolderIndex !== -1 && filePath.substring(subfolderIndex).split("/").length === 2;
  }),
  false,
  "Select source of annotation/citation"
));

var selectedFile = await suggestions.file;
var annotation = await selectedFile.basename;
