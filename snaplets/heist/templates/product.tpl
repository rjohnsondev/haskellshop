<apply template="_base">
<div class="container">
  <ol class="breadcrumb" style="margin-top: 20px;">
    <li><a href="/browse">Browse</a></li>
    <category_crumbtrail>
        <li><a href="browse?c=${category_id}" class="${category_class}"><category_name /></a></li>
    </category_crumbtrail>
  </ol>
</div>


<div class="container">
  <div class="row">
    <div class="col-md-12">
      <alerts>
        <div class="alert alert-${level}" role="alert"><msg /></div>
      </alerts>
    </div>
  </div>
</div>

<!-- Example row of columns -->
<product>
<div class="container">
  <div class="row">
    <div class="col-md-6">
      <div class="container-fluid">
        <form method="post" action="">
          <category>
            <input type="hidden" name="c" value="${category_id}" />
          </category>
          <!-- Example row of columns -->
          <div class="row">
            <div class="col-md-12">
              <hrgroup>
                <h3 class="product-name"><name /></h3>
                <h4><manufacturer/></h4>
              </hrgroup>
              <p><span class="price"><base_price /></span></p>
            </div>
          </div>
          <div class="row">
            <variants_filtered>
              <div class="col-md-12">
                <div class="form-group">
                  <label for="sel1"><variant_name /></label>
                  <select class="form-control" name="variant_options" id="sel1">
                    <variant_options>
                      <option value="${variant_option_id}">
                        <variant_option_name />
                        <small><variant_option_price /></small>
                      </option>
                    </variant_options>
                  </select>
                </div>
              </div>
            </variants_filtered>
          </div>
          <div class="row">
            <div class="col-md-12">
                <input type="hidden" name="product_id" value="${id}" />
                <button type="submit" class="btn btn-primary btn-lg btn-block">Add to Basket</button>
            </div>
          </div>
          <br>
        </form>
      </div>
      <div class="panel with-nav-tabs panel-default">
        <div class="panel-heading">
          <ul class="nav nav-tabs" role="tablist">
            <details>
              <li role="presentation" class="${active_class}">
                <a href="#detail-${id}" aria-controls="detail-${id}" role="tab" data-toggle="tab">
                  <title />
                </a>
              </li>
            </details>
          </ul>
        </div>
        <div class="panel-body">
          <div class="tab-content">
            <details>
              <div role="tabpanel" class="tab-pane ${active_class}" id="detail-${id}">
                <detail_html />
              </div>
            </details>
          </div>
        </div>
      </div>

    </div>
    <div class="col-md-6">
      <img src="/static/img/products/${image_hash}_orig.jpg" id="product-image" class="img-rounded img-responsive" />
      <hr />
      <images>
        <img src="/static/img/products/${hash}_sml.jpg" data-hash="${hash}" class="img-thumbnail clickable" />
      </images>
    </div>
  </div>
</div>
</product>
<script>
$('.img-thumbnail').click(function(e) {
  $("#product-image").attr("src", "/static/img/products/" + $(this).data("hash") + "_orig.jpg");
});
</script>
</apply>
