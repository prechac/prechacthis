downloadJoePass();

function downloadJoePass() {
    var joe_f = $('joepass_form');
    if($F(joe_f['allways']) == 'on') {
        var url = "./joe.pass?"+joe_f.serialize();
        window.location.href = url;
    }
}

