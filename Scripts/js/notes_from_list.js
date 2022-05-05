<%*

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

var fileName =

const fileName = await tp.system.prompt("File name")
const templateName = await tp.system.suggester(["Template1", "Template2"], ["Template1", "Template2"])
tp.file.create_new(tp.file.find_tfile(templateName), fileName, tp.file.folder())


if (manual_title_p == "false") {
let postfix_size = 3;//the size of string 
let alphabet = "abcdefghijkmnpqrstuvwxyz"; //from where to create
let postfix = "";
for ( var i=0; i < postfix_size; i++ )
        postfix += alphabet[Math.floor(Math.random() * alphabet.length)];
let prefix = tp.date.now("YYMMDD");


%>
