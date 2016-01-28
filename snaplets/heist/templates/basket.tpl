<apply template="_base">
<div class="container">
  <ol class="breadcrumb" style="margin-top: 20px;">
    <li><a href="/">Home</a></li>
    <li>Your Basket</li>
  </ol>
</div>

<div class="container">
  <div class="row">
    <div class="col-md-12">
      <div class="panel panel-default">
        <div class="panel-heading">
          <h3 class="panel-title">Basket</h3>
        </div>
        <basket>
          <div class="panel-body text-center" style="${empty_basket_class}">
            <a href="/"><img src="/static/img/empty_basket.jpg" alt="Empty Basket!" /></a>
          </div>
          <div class="panel-body" style="${non_empty_basket_class}">
            <div class="pull-right" style="padding-bottom: 20px;">
              <form action="/payment">
                <button class="btn btn-default">
                  Continue
                  <i class="glyphicon glyphicon-chevron-right"></i>
                </button>
              </form>
            </div>
            <table class="table">
              <tr>
                <th>Item</th>
                <th>Quantity</th>
                <th>Price</th>
              </tr>
              <basket_items>
                <tr>
                  <td>
                    <img src="/static/img/products/${product_hash}_sml.jpg" class="pull-left" style="padding-right: 10px" />
                    <h4><product_name /> <small><product_manufacturer /></small></h4>
                    <product_options /><br />
                  </td>
                  <td>
                    <form action="" method="post">
                    <button
                      type="submit"
                      class="glyphicon glyphicon-chevron-up btn btn-primary btn-xs"
                      name="basket-increase"
                      value="${product_basket_key}"></button>
                    <strong style="padding: 2px;"><product_quantity /></strong>
                    <button
                      type="submit"
                      class="glyphicon glyphicon-chevron-down btn btn-primary btn-xs"
                      name="basket-decrease"
                      value="${product_basket_key}"></button>
                    </form>
                  </td>
                  <td>
                    <product_price />
                  </td>
                </tr>
              </basket_items>
              <tr>
                <th></th>
                <th>Total</th>
                <th><basket_total /></th>
              </tr>
            </table>
            <div class="pull-right" style="padding-bottom: 20px;">
              <form action="/payment">
                <button class="btn btn-default">Continue <i class="glyphicon glyphicon-chevron-right"></i></button>
              </form>
            </div>
          </div>
        </basket>
      </div>
    </div>
  </div>
</div>
</apply>
