<apply template="_base">
<style>
.img-thumbnail {
  margin-bottom: 5px;
  padding: 5px; /* why does 4 distort? */
}
</style>

<div class="container" style="margin-top: 20px;">
  <ol class="breadcrumb">
    <li class="active"><a href="/admin/products">Products</a></li>
    <li class="active">Modify Product</li>
  </ol>
</div>

<!-- Example row of columns -->
<div class="container">
  <div class="row">
    <div class="col-md-12">
      <apply template="_err"/>
    </div>
  </div>
  <div class="row">
    <form method="post" action="" enctype="multipart/form-data">
      <div class="col-md-6">
        <product>
          <div class="panel panel-default">
            <div class="panel-heading">Product</div>
            <div class="panel-body">
              <input type="hidden" name="product-id" value="${id}">
              <div class="checkbox">
                <label>
                  <enabled_checkbox>
                    <input
                      name="product-enabled"
                      type="checkbox"
                      data-on="Is"
                      data-size="small"
                      data-off="Not"
                      data-toggle="toggle"
                      data-offstyle="danger"
                      data-onstyle="default">
                  </enabled_checkbox>
                  available for purchase.
                </label>
              </div>
              <div class="form-group">
              </div>
              <div class="form-group">
                <label class="control-label" for="name">Name</label>
                <input type="text" class="form-control" name="product-name" value="${name}" id="name">
              </div>
              <div class="form-group">
                <label class="control-label" for="name">Manufacturer</label>
                <input type="text" class="form-control" name="product-manufacturer" value="${manufacturer}" id="manufacturer">
              </div>
              <div class="form-group">
                <label class="control-label" for="base-price">Base Price</label>
                <input type="text" class="form-control" id="base-price" name="product-base-price" value="${base_price}">
              </div>
              <div class="form-group" style="${hide_new_product_class}">
                <label class="control-label" for="base-image">Image</label>
                <input type="file" class="form-control" id="base-image" name="product-image" />
              </div>
              <div class="form-group" style="${hide_new_product_class}">
                <label class="control-label" for="base-image">
                  Product Details
                  <small>(<a href="https://daringfireball.net/projects/markdown/syntax">use markdown</a>)</small>
                </label>
                <textarea rows="5" class="form-control" id="base-details" name="product-details"></textarea>
              </div>
              <div class="form-group">
                <label class="control-label" style="margin-bottom: 0px;">Categories</label>
                <category_list>
                  <div class="checkbox">
                    <label>
                      <category_checkbox><input type="checkbox" name="categories" /></category_checkbox>
                      <category_name />
                    </label>
                  </div>
                </category_list>
              </div>
              <button class="btn btn-primary" name="product-save" value="1" type="submit">Save Product</button>
            </div>
          </div>

          <div class="panel panel-default" style="${hide_product_images}">
            <div class="panel-heading">Images</div>
            <div class="panel-body">
              <images>
                <div class="img-thumbnail">
                  <img src="/static/img/products/${hash}_sml.jpg" />
                  <div style="text-align: center; border-top: 1px solid #ddd; padding-top: 5px;">
                    <button
                      class="glyphicon glyphicon-chevron-left btn btn-primary btn-xs"
                      name="image-move-up"
                      value="${hash}"></button>
                    <button
                      class="glyphicon glyphicon-remove btn btn-danger btn-xs del-btn image-delete"
                      name="image-delete"
                      value="${hash}"></button>
                    <button
                      class="glyphicon glyphicon-chevron-right btn btn-primary btn-xs"
                      name="image-move-down"
                      value="${hash}"></button>
                  </div>
                </div>
              </images>
              <hr />
              <div class="form-group">
                <label class="control-label" for="additional-image" style="margin-bottom: 0px;">Add Image:</label><br />
                <input type="file" name="product-image" id="additional-image" style="padding-top: 5px;"/>
              </div>
              <button class="btn btn-default" name="product-save-image" value="1" type="submit">Add Image</button>
            </div>
          </div>

          <div class="panel panel-default" style="${hide_product_images}">
            <div class="panel-heading">
              Details
              <small>(<a href="https://daringfireball.net/projects/markdown/syntax">use markdown</a>)</small>
            </div>
            <div class="panel-body">
              <details>
                <div class="form-group">
                  <input name="product-detail-title-${id}" type="text" class="form-control" value="${title}" />
                  <textarea name="product-detail-detail-${id}" class="form-control" rows="5"><detail /></textarea>
                </div>
                <button class="btn btn-default" name="product-detail-save" value="${id}" type="submit">Save</button>
                <div class="pull-right">
                  <button
                    class="glyphicon glyphicon-chevron-up btn btn-primary btn-xs"
                    name="product-detail-move-up"
                    value="${id}"></button>
                  <button
                    class="glyphicon glyphicon-remove btn btn-danger btn-xs del-btn image-delete"
                    name="product-detail-delete"
                    value="${id}"></button>
                  <button
                    class="glyphicon glyphicon-chevron-down btn btn-primary btn-xs"
                    name="product-detail-move-down"
                    value="${id}"></button>
                </div>
                <hr />
              </details>
              <div class="form-group">
                <label class="control-label">Add Detail:</label>
                <input name="product-detail-title-0" type="text" class="form-control" />
                <textarea class="form-control" name="product-detail-detail-0" rows="5"></textarea>
              </div>
              <button class="btn btn-default" name="product-detail-save" value="0" type="submit">Add Section</button>
            </div>
          </div>

        </product>

      </div>
      <div class="col-md-6">
        <variants>
        <div class="panel panel-default">
          <div class="panel-heading">
            <variant_name />
            <div class="pull-right">
              <variant_enabled_checkbox>
                <input type="checkbox" class="variant-enable" data-toggle="toggle" data-size="mini">
              </variant_enabled_checkbox>
            </div>
          </div>
          <div class="panel-body variant-container" id="container-${variant_id}">
            <div class="checkbox">
              <variant_options>
              <div style="padding: 5px;">
                <label>
                  <variant_option_enabled_checkbox>
                    <input type="checkbox" class="variant-option-enable" data-target="populated-at-runtime" name="product-variant-options">
                  </variant_option_enabled_checkbox>
                  <variant_option_name />
                </label>
                <variant_option_price_block>
                  <input type="price" class="form-control" placeholder="Price Adjustment" />
                </variant_option_price_block>
              </div>
              </variant_options>
            </div>
          </div>
        </div>
        </variants>
      </div>
    </form>
  </div>
</div>

<script>
$(function() {
  $('.variant-enable').each(
    function() {
      if (!$(this)[0].checked) {
        $($(this).data("target")).hide();
      }
      $(this).change(function() {
        $($(this).data("target")).slideToggle();
      });
    }
  );
  $('.variant-option-enable').change(function() {
    $($(this).data("target")).prop('disabled', !this.checked);
  });
  $('.image-delete').click(function() {
    return confirm('Are you sure you want to delete this?');
  });
})
</script>
</apply>
