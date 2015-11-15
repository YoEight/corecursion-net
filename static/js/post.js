function submitPost() {
    $.ajax({
        url: "/admin/post",
        method: "POST",
        data: $('form#post-form').serialize(),
        success: function() {
            window.location = "/admin";
        }
    });
}

function showDeleteModal() {
    $('#delete-modal').modal('show');
}

function showPublishModal() {
    $('#publish-modal').modal('show');
}

function showUnpublishModal() {
    $('#unpublish-modal').modal('show');
}

function deletePost(pid) {
    $.ajax({
        url: "/admin/post/" + pid,
        method: "DELETE"
    }).done(function(){
        window.location = "/admin";
    });
}

function publishPost(pid) {
    var date = $('#post-date').val();
    var data = "";

    if (date != "") {
        data = "post-date=" + date;
    }

    $.ajax({
        url: "/admin/post/" + pid,
        method: "PUT",
        data: data
    }).done(function(){
        window.location = "/admin";
    });
}

function unpublishPost(pid) {
    $.ajax({
        url: "/admin/post/unpublish/" + pid,
        method: "PUT"
    }).done(function(){
        window.location = "/admin";
    });
}
