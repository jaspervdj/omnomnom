function _cart(name) {
    $.post("/_cart/", {name: name}, function (result) {
        $("#cart").html(result);
    });

    return false;
}
