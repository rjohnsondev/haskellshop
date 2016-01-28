<apply template="_base">
<div class="container" style="margin-top: 20px;">
  <ol class="breadcrumb">
    <li class="active">Orders</li>
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
    <div class="col-md-12">
      <div class="panel panel-default">
        <div class="panel-heading">Orders</div>
        <div class="panel-body">
          <div class="pull-right">
            <form action="" method="get" class="form-inline">
              <select name="orders_range" class="form-control">
                <orders_date_range_options />
              </select>
              <button class="btn btn-default">View</button>
            </form>
            <br />
          </div>
          <table class="table">
            <tr>
              <th>Order #</th>
              <th>Email</th>
              <th>Status</th>
              <th>Total</th>
              <th></th>
            </tr>
            <orders>
                <tr>
                  <td><order_id/></td>
                  <td><order_email /></td>
                  <td><order_processed /></td>
                  <td><order_total /></td>
                  <td>
                    <form action="/admin/order" method="get" class="form-inline">
                      <button class="btn btn-default" name="order_id" value="${order_id}">
                        View Order
                        <i class="glyphicon glyphicon-chevron-right"></i>
                      </button>
                    </form>
                  </td>
                </tr>
            </orders>
          </table>
        </div>
      </div>
    </div>
  </div>
</div>

</apply>
