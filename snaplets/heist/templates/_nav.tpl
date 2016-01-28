<nav class="navbar navbar-inverse navbar-fixed-top" role="navigation">
<div class="container">
  <div class="navbar-header">
    <div class="collapsed" aria-expanded="false" aria-controls="navbar">
      <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar">
        <span class="sr-only">Toggle navigation</span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </button>
      <a href="/basket" style="display: block; float: right; color: #9d9d9d; padding: 13px;" class="hidden-md hidden-lg hidden-sm">
        <span class="glyphicon glyphicon-shopping-cart cart"></span>
        <span class="badge"><basket><basket_count /></basket></span>
      </a>
    </div>
    <a class="navbar-brand" id="nav-bar-title" href="/">Pets&#x2019; Paradise &#x1f43e;</a>
  </div>
  <div id="navbar" class="navbar-collapse collapse">
    <ul class="nav navbar-nav navbar-left">
      <top_level_categories>
        <li><a href="/browse?c=${category_id}"><category_name /></a></li>
      </top_level_categories>
    </ul>
    <ul class="nav navbar-nav navbar-right hidden visible-lg">
      <li>
        <a href="/basket">
          <span class="glyphicon glyphicon-shopping-cart cart"></span>
          <span class="badge"><basket><basket_count /></basket></span>
        </a>
      </li>
    </ul>
  </div><!--/.navbar-collapse -->
</div>
</nav>
