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
