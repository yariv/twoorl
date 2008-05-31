var maxChars = 140;

$(function() {
    $("#input1").focus();
    $("#txt").focus().keypress(function(e) {
	if (e.which == 13) { //enter
	    send();
	    return false;
	}
	var length = val().length+1;
	$("#chars").text("" + (maxChars - length));
	
    });
});

function val() {
    return $("#txt")[0].value;
}

function send() {
    var msg = val();
    if (msg.length > 0) {
	$("#txt")[0].value = "";
	$("#chars").text("" + maxChars);
	$.post("/api/send", {"msg": msg, "get_html": "true"},
	       function(html) {
		   $("#placeholder").prepend(html);
		   $("#txt").focus();
	       });
    }
}

function follow(username, val) {
    $.post("/api/follow",
	   {"username": username,
	    "value": val},
	   function(Res) {
	       if (val == "1") {
		   $("#follow").hide();
		   $("#unfollow").show();
	       } else {
		   $("#follow").show();
		   $("#unfollow").hide();
	       }
	   });
	   
}
