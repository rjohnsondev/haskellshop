<apply template="_base">
<div class="container">
  <ol class="breadcrumb" style="margin-top: 20px;">
    <li><a href="/">Home</a></li>
    <li>Payment Details</li>
  </ol>
</div>

<payment>
<form action="" method="post">
  <div class="container">
    <div class="row">
      <div class="col-md-12">
        <apply template="admin/_err"/>
      </div>
    </div>
    <div class="row">
      <div class="col-sm-6">
        <div class="panel panel-default">
          <div class="panel-heading">Billing Details</div>
          <div class="panel-body">
            <div class="form-horizontal">
              <div class="form-group">
                <div class="col-sm-12 ${billing_full_name_class}" id="billing-full-name-container">
                  <input
                    id="billing-full-name"
                    name="billing_full_name"
                    type="text"
                    value="${billing_full_name}"
                    placeholder="full name *"
                    class="form-control">
                </div>
              </div>
              <div class="form-group">
                <div class="col-sm-12 ${billing_address_1_class}">
                  <input
                    id="billing-address-1"
                    name="billing_address_1"
                    type="text"
                    value="${billing_address_1}"
                    placeholder="address line 1 *"
                    class="form-control">
                  <p class="help-block">Street address, P.O. box, company name, c/o</p>
                </div>
                <div class="col-sm-12">
                  <input
                    id="billing-address-2"
                    name="billing_address_2"
                    type="text"
                    value="${billing_address_2}"
                    placeholder="address line 2"
                    class="form-control">
                  <p class="help-block">Apartment, suite , unit, building, floor, etc.</p>
                </div>
                <div class="col-sm-12 ${billing_city_class}">
                  <input
                    id="billing-city"
                    name="billing_city"
                    type="text"
                    value="${billing_city}"
                    placeholder="city / town *"
                    class="form-control">
                  <p class="help-block"></p>
                </div>
                <div class="col-sm-5">
                  <input
                     id="billing-state"
                     name="billing_state"
                     type="text"
                     value="${billing_state}"
                     placeholder="state / province / region"
                     class="form-control">
                  <p class="help-block"></p>
                </div>
                <div class="col-sm-3">
                  <input
                     id="billing-postcode"
                     name="billing_postcode"
                     type="text"
                     value="${billing_postcode}"
                     placeholder="zip / postcode"
                     class="form-control">
                  <p class="help-block"></p>
                </div>
                <div class="col-sm-4 ${billing_country_class}">
                  <select id="billing-country" name="billing_country" class="form-control">
                    <option value="" selected="selected">- Country * -</option>
                    <billing_countries />
                  </select>
                </div>
                <div class="col-sm-12 ${email_class}">
                  <input
                    id="email"
                    name="email"
                    type="text"
                    value="${email}"
                    placeholder="email *"
                    class="form-control">
                  <p class="help-block"></p>
                </div>
                <div class="col-sm-12 ${phone_class}">
                  <input
                    id="phone"
                    name="phone"
                    type="text"
                    value="${phone}"
                    placeholder="phone (inc. country code) *"
                    class="form-control">
                  <p class="help-block"></p>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
      <div class="col-md-1 text-center">
        <br>
        <span class="glyphicon glyphicon-option-vertical"></span><br>
        <br />
        <button class="btn btn-default" type="button" id="btn-copy">
          Copy
          <i class="glyphicon glyphicon-chevron-right"></i>
        </button><br/>
        <br />
        <span class="glyphicon glyphicon-option-vertical"></span><br>
        <br>
      </div>
      <div class="col-sm-5">
        <div class="panel panel-default">
          <div class="panel-heading">Shipping Details</div>
          <div class="panel-body">
            <div class="form-horizontal">
              <div class="form-group">
                <div class="col-sm-12 ${shipping_full_name_class}">
                  <input
                     id="shipping-full-name"
                     name="shipping_full_name"
                     value="${shipping_full_name}"
                     type="text"
                     placeholder="full name *"
                     class="form-control">
                </div>
              </div>
              <div class="form-group">
                <div class="col-sm-12 ${shipping_address_1_class}">
                  <input
                     id="shipping-address-1"
                     name="shipping_address_1"
                     value="${shipping_address_1}"
                     type="text"
                     placeholder="address line 1 *"
                     class="form-control">
                  <p class="help-block">Street address, P.O. box, company name, c/o</p>
                </div>
                <div class="col-sm-12">
                  <input
                     id="shipping-address-2"
                     name="shipping_address_2"
                     value="${shipping_address_2}"
                     type="text"
                     placeholder="address line 2"
                     class="form-control">
                  <p class="help-block">Apartment, suite , unit, building, floor, etc.</p>
                </div>
                <div class="col-sm-12 ${shipping_city_class}">
                  <input
                    id="shipping-city"
                    name="shipping_city"
                    value="${shipping_city}"
                    type="text"
                    placeholder="city / town *"
                    class="form-control">
                  <p class="help-block"></p>
                </div>
                <div class="col-sm-5">
                  <input
                    id="shipping-state"
                    name="shipping_state"
                    value="${shipping_state}"
                    type="text"
                    placeholder="state / province / region"
                    class="form-control">
                  <p class="help-block"></p>
                </div>
                <div class="col-sm-3">
                  <input
                    id="shipping-postcode"
                    name="shipping_postcode"
                    value="${shipping_postcode}"
                    type="text"
                    placeholder="zip / postcode"
                    class="form-control">
                  <p class="help-block"></p>
                </div>
                <div class="col-sm-4 ${shipping_country_class}">
                  <select id="shipping-country" name="shipping_country" class="form-control">
                    <option value="" selected="selected">- Country * -</option>
                    <shipping_countries />
                  </select>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
    <div class="row">
      <div class="col-sm-7">
        <div class="panel panel-default">
          <div class="panel-heading">You are purchasing</div>
          <div class="panel-body">
            <basket>
            <basket_items>
            <div class="row">
              <div class="col-md-3 text-center">
                <img src="/static/img/products/${product_hash}_sml.jpg"/>
              </div>
              <div class="col-md-9">
                <strong><product_name /></strong><br />
                <product_manufacturer /><br />
                <product_options /><br />
                <product_price />
              </div>
            </div>
            <hr>
            </basket_items>
            <div class="pull-right">
              <strong>Basket Total:</strong>
              <basket_total />
            </div>
            </basket>
          </div>
        </div>
      </div>
      <div class="col-sm-5">
        <div class="panel panel-default">
          <div class="panel-heading">Payment Details</div>
          <div class="panel-body">
            <div class="form-horizontal">
              <div class="form-group">
                <label for="ccname" class="control-label col-sm-3">Card Name:</label>
                <div class=" col-sm-9">
                  <input type="text" class="form-control" name="ccname" id="ccname">
                </div>
              </div>
              <div class="form-group">
                <label for="ccnum" class="control-label col-sm-3">Card Num:</label>
                <div class=" col-sm-9">
                  <input type="text" class="form-control" id="ccnum" placeholder="DEMO STORE, DO NOT ENTER REAL CARDS" name="ccnum" autocomplete="off">
                </div>
              </div>
              <div class="form-group">
                <label for="ccmonth" class="control-label col-sm-3">Expiry Date:</label>
                <div class="col-sm-5 form-inline">
                  <input type="text" class="form-control" name="ccmonth" id="ccmonth" size="2" maxlength="2"> /
                  <input type="text" class="form-control" name="ccyear" id="ccyear" size="2" maxlength="2">
                </div>
                <label for="ccv" class="control-label col-sm-1">CCV:</label>
                <div class="col-sm-3">
                  <input type="text" class="form-control" name="ccv" id="ccv" autocomplete="off">
                </div>
              </div>
              <button class="btn btn-block btn-primary" type="submit">Buy Now!</button>
            </div>
          </div>
        </div>
      </div>
    </div>
  </div>
</form>
</payment>
<script>
$('#btn-copy').click(function(e) {
  $('#shipping-full-name').val(
      $('#billing-full-name').val()).blur();
  $('#shipping-address-1').val(
      $('#billing-address-1').val()).blur();
  $('#shipping-address-2').val(
      $('#billing-address-2').val()).blur();
  $('#shipping-city').val(
      $('#billing-city').val()).blur();
  $('#shipping-state').val(
      $('#billing-state').val()).blur();
  $('#shipping-postcode').val(
      $('#billing-postcode').val()).blur();
  $('#shipping-country').val(
      $('#billing-country').val()).blur();
});
req_fields = ["billing-full-name", "billing-address-1", "billing-city",
              "billing-country", "shipping-full-name", "shipping-address-1",
              "shipping-city", "shipping-country", "email", "phone"];
for (x = 0; x < req_fields.length; x++) {
    $("#" + req_fields[x]).blur(function (e) {
        var el = $(this);
        if (el.val() != "") {
            el.parent().removeClass("has-error");
        }
    });
}

</script>
</apply>
