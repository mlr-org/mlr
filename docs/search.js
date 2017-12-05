$.getJSON("site-index.json", function(json) {

    window.store = [];
    window.index = lunr(function() {
      this.ref("href");
      this.field("type");
      this.field("title");
      this.field("words", { boost: 10 });

      json.forEach(function (doc) {
        this.add(doc);
        window.store[doc.href] = {'title': doc.title, 'type': doc.type} ;
      }, this);
    });
  });


$(document).ready(function() {

    query = window.location.search.split("=")[1];

    result = index.search(query);

    resultdiv = $('ul.search-results');

    resultdiv.empty();
    resultdiv.prepend('<p class="">Found ' + result.length + ' result(s)</p>');

    for (var item in result) {
      ref = result[item].ref;
      searchitem = '<li><a href="' + ref + '">' + store[ref].title + '</a> (' + store[ref].type + ')</li>';
      resultdiv.append(searchitem);
    }

});
