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
