<apply template="_base">

<div class="container" style="margin-top: 20px;">
  <ol class="breadcrumb">
    <li class="active">Products</li>
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
        <div class="panel-heading">Products</div>
        <div class="panel-body">
          <table class="table">
            <tr>
              <th>#</th>
              <th>Image</th>
              <th>Name</th>
              <th>Price</th>
              <th></th>
            </tr>
            <product_list>
              <tr>
                <td><product_id /></td>
                <td><img src="http://localhost:8000/static/img/products/${product_hash}_sml.jpg" /></td>
                <td><product_name /></td>
                <td><product_base_price /></td>
                <td>
                  <a href="/admin/new_product?product_id=${product_id}" class="btn btn-default">Edit</a>
                </td>
              </tr>
            </product_list>
          </table>
          <a class="btn btn-default" href="new_product" role="button">Add New Product</a>
        </div>
      </div>
    </div>
  </div>
</div>

</apply>
