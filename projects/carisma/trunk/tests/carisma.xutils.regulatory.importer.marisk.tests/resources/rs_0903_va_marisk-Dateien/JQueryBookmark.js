function addBookmark(title, url, obj){
if (!url) url = location.href;
if (!title) title = document.title;
if ((typeof window.sidebar == "object") && (typeof window.sidebar.addPanel == "function"))
window.sidebar.addPanel (title, url, "");//Gecko
else if (document.all)
window.external.AddFavorite(url, title); //IE4+
else if (window.opera && document.createElement) {
obj.setAttribute('rel','sidebar');
obj.setAttribute('href',url);
obj.setAttribute('title',title);
}
else {
alert("Leider funktioniert die Lesezeichen-Aktion nicht in Ihrem Browser.\nDrücken Sie bitte STRG+D");
return false; //IF Opera 6
}
return true;
}