// This demo uses jQuery UI Autocomplete
// https://jqueryui.com/autocomplete

// Cannot style datalist elements yet, so get
// each option value and pass to jQuery UI Autocomplete
$('input[data-list]').each(function () {
    var availableTags = $('#' + $(this).attr("data-list")).find('option').map(function () {
        return this.value;
    }).get();

    $(this).autocomplete({
        source: availableTags
    }).on('focus', function () {
        $(this).autocomplete('search', ' ');
    }).on('search', function () {
        if ($(this).val() === '') {
            $(this).autocomplete('search', ' ');
        }
    });
});


$(function () {
    var availableTutorials = [
        "M_001",
        "M_002",
        "M_003",
        "M_004",
    ];
    $("#automplete-1").autocomplete({
        source: availableTutorials
    });
});

$(function () {
    var availableTutorials2 = [
        "Cl_001",
        "Cl_002",
        "Cl_003",
        "CL_004",
        "M_002",
        "M_003",
        "M_004",
    ];
    $("#automplete-2").autocomplete({
        source: availableTutorials2
    });
});


$(function () {
    var availableTutorials3 = [
        "3",
        "6",
        "9",
        "12",
    ];
    $("#automplete-3").autocomplete({
        source: availableTutorials3
    });
});