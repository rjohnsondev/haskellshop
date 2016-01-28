<apply template="_base">

<div class="container" style="margin-top: 20px;">
  <ol class="breadcrumb">
    <li class="active"><a href="/admin/">Orders</a></li>
    <li class="active">View Order</li>
  </ol>
</div>

<order>
  <div class="container">
    <div class="row">
      <div class="col-sm-6">
        <div class="panel panel-default">
          <div class="panel-heading">Billing Details</div>
          <div class="panel-body">
            <billing_address>
                <name />
                <address>
                  <line_1/><br />
                  <line_2/><br />
                  <city />, <state />, <postcode />, <country />
                </address>
            </billing_address>
          </div>
        </div>
      </div>
      <div class="col-sm-6">
        <div class="panel panel-default">
          <div class="panel-heading">Shipping Details</div>
          <div class="panel-body">
            <shipping_address>
                <name />
                <address>
                  <line_1/><br />
                  <line_2/><br />
                  <city />, <state />, <postcode />, <country />
                </address>
            </shipping_address>
          </div>
        </div>
      </div>
    </div>
    <div class="row">
      <div class="col-sm-9">
        <div class="panel panel-default">
          <div class="panel-heading">Has Purchased</div>
          <div class="panel-body">
            <items>
              <div class="row">
                <div class="col-md-3 text-center">
                  <img src="/static/img/products/${hash}_sml.jpg"/>
                </div>
                <div class="col-md-9">
                  <strong><name /></strong><br />
                  <manufacturer /><br />
                  <options /><br />
                  <price />
                </div>
              </div>
              <hr>
            </items>
            <div class="text-right">
              <strong>Total Paid:</strong>
              <total /><br/>
              <strong>Payment Gateway Ref #:</strong>
              12345
            </div>
          </div>
        </div>
      </div>
      <div class="col-sm-3">
        <form action="" method="post" style="${not_processed_style}">
          <button class="btn btn-primary btn-block" id="processed" type="submit" name="order_id" value="${order_id}">Mark Order as Processed</button>
        </form>
        <div style="${processed_style}">
          <strong>Processed:</strong><br/>
          <processed_time />
        </div>
      </div>
    </div>
  </div>
</order>
<script>
$('#processed').click(function(e) {
  return confirm('Are you sure you wish to mark this order as processed?');
});
</script>
</apply>

