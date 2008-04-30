function mymetas(){
document.myform.txtarea.value='<TITLE>'+document.myform.title.value+'</TITLE>\n'+
'<META NAME="TITLE" CONTENT="'+document.myform.title.value+'">\n'+
'<META NAME="DESCRIPTION" CONTENT="'+document.myform.description.value+'">\n'+
'<META NAME="KEYWORDS" CONTENT="'+document.myform.keywords.value+'">\n'+
'<META NAME="ROBOTS" CONTENT="all">\n'+
'<META NAME="AUTHOR" CONTENT="'+document.myform.title.value+'">\n'+
'<META HTTP-EQUIV="KEYWORDS" NAME="KEYWORDS" CONTENT="'+document.myform.title.value+'">\n'
document.myform.btn.value='Create Meta Tags'
}

function CountWords (this_field, show_word_count, show_char_count, checkok) {
if (show_word_count == null) {
show_word_count = true;
}
if (show_char_count == null) {
show_char_count = false;
}
var char_count = this_field.value.length;
var fullStr = this_field.value + " ";
var initial_whitespace_rExp = /^[^A-Za-z0-9]+/gi;
var left_trimmedStr = fullStr.replace(initial_whitespace_rExp, "");
var non_alphanumerics_rExp = rExp = /[^A-Za-z0-9]+/gi;
var cleanedStr = left_trimmedStr.replace(non_alphanumerics_rExp, " ");
var splitString = cleanedStr.split(" ");
// var word_count = splitString.length -1;
var word_count = this_field.value.split(/\s+/).length //- 1
if (fullStr.length <2) {
word_count = 0;
}
if (word_count == 1) {
wordOrWords = " word";
}
else {
wordOrWords = " words";
}
if (char_count == 1) {
charOrChars = " character";
} else {
charOrChars = " characters";
}
if (show_word_count & show_char_count) {
if (checkok == 0) {
alert ("Word Count:\n" + "    " + word_count + wordOrWords + "\n" + "    " + char_count + charOrChars+ "\n\n" + "Maximum is 500 words");
} else {
if (word_count > 500) {
alert ("Too many words, unacceptable:\n" + "    " + word_count + wordOrWords + "\n" + "    " + char_count + charOrChars+ "\n\n" + "Maximum is 500 words");
}
}
}
else {
if (show_word_count) {
alert ("Word Count:  " + word_count + wordOrWords);
}
else {
if (show_char_count) {
alert ("Character Count:  " + char_count + charOrChars);
      }
   }
}
if (checkok == 0) {
return word_count;
} else {
return word_count < 501;
}
}
