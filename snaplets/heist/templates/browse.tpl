<apply template="_base">

<div class="container">
  <ol class="breadcrumb" style="margin-top: 20px;">
    <li><a href="/browse">Browse</a></li>
    <category_crumbtrail>
        <li><a href="browse?c=${category_id}" class="${category_class}"><category_name /></a></li>
    </category_crumbtrail>
  </ol>
</div>

<!-- Example row of columns -->
<div class="container">
  <div class="row">
    <category>
      <div class="col-md-3">
        <nav class="navbar-default">

          <div class="container-fluid">
            <!-- Brand and toggle get grouped for better mobile display -->
            <div class="navbar-header">
              <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-2" aria-expanded="false">
                <span class="sr-only">Toggle navigation</span>
                <span class="glyphicon glyphicon-chevron-down"></span>
              </button>
              <span class="navbar-brand">Refine</span>
            </div>
          </div>

          <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-2">

            <div class="panel panel-default" style="${hide_categories}">
              <div class="panel-heading">Category</div>
              <div class="panel-body">
                <ul>
                  <categories>
                      <li><a href="browse?c=${category_id}"><category_name /></a></li>
                  </categories>
                </ul>
              </div>
            </div>

            <form method="get" action="browse" id="browse_form">
              <input type="hidden" name="c" value="${category_id}" />

              <div class="panel panel-default">
                <div class="panel-heading">Price</div>
                <div class="panel-body" style="padding: 10px;">
                  <price_filter>
                  <input
                     id="ex2"
                     type="text"
                     class="form-control"
                     name="p"
                     value="${selected_min},${selected_max}"
                     data-slider-min="${min}"
                     data-slider-max="${max}"
                     data-slider-step="2"
                     data-slider-value="[${selected_min},${selected_max}]"/>
                  <div class="pull-left">£<min /></div>
                  <div class="pull-right">£<max /></div>
                  </price_filter>
                </div>
              </div>

              <variants_url_selected>
                <div class="panel panel-default">
                  <div class="panel-heading"><variant_name /></div>
                  <div class="panel-body" style="padding: 0 15px;">
                    <variant_options>
                      <div class="checkbox">
                        <label>
                          <variant_option_enabled_checkbox>
                            <input type="checkbox" class="variant_option" name="vo" />
                          </variant_option_enabled_checkbox>
                          <variant_option_name />
                        </label>
                      </div>
                    </variant_options>
                  </div>
                </div>
              </variants_url_selected>

              <button type="submit" id="update-btn" class="btn btn-default btn-block" style="margin-bottom: 10px;">
                Update Results
              </button>

            </form>

          </div>
        </nav>
      </div>
      <div class="col-md-9">
        <nav class="navbar-default">
          <div class="container-fluid">
            <!-- Brand and toggle get grouped for better mobile display -->
            <div class="navbar-header">
              <category>
                <span class="navbar-brand"><category_name /></span>
              </category>
            </div>
          </div>
        </nav>

        <div class="container-fluid">
          <div class="row">
            <br />
            <product_list>
              <div class="col-md-3 text-center product">
                <a href="/product?c=${category_id}&product_id=${product_id}">
                  <img src="/static/img/products/${product_hash}_sml.jpg" />
                  <hgroup>
                    <h4 class="product-name"><product_name /></h4>
                    <h5><product_manufacturer /></h5>
                  </hgroup>
                  <div class="price"><product_base_price /></div>
                </a>
              </div>
            </product_list>
          </div>
          <hr />
        </div>

      </div>
    </category>
  </div>
</div>
<script>
// Without JQuery
var mySlider = $("#ex2").slider();
mySlider.on("slideStop", function(oldval, newval) {
  $('#browse_form').submit()
});

$('.variant_option').change(function(e) {
  $('#browse_form').submit()
});

</script>
</apply>
