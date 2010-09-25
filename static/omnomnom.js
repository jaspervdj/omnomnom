function appendBody(url) {
    var result = $.ajax({url: url, async: false}).responseText;
    $("body").append(result);
}

function addUser() {
    var name = $("#add-user-name").val();
    $("#users").html("Adding user...");
    $.post("/add-user/", {name: name}, function (result) {
        $("#users").html(result);
    });

    return false;
}

function order(name) {
    $.post("/order/", {name: name}, function (result) {
        $("#cart").html(result);
    });

    return false;
}
