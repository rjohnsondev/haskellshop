<apply template="_base">
<!-- Example row of columns -->
<style>
.cat {
  border: 1px solid #ddd;
  border-radius: 4px;
  padding: 10px;
  margin: 10px 0 0 0;
}
</style>

<div class="container" style="margin-top: 20px;">
  <ol class="breadcrumb">
    <li class="active">Categories</li>
  </ol>
</div>

<div class="container">
  <div class="row">
    <div class="col-md-12">
      <apply template="_err"/>

      <div class="panel panel-default">
        <div class="panel-heading">
          Categories
        </div>
        <div class="panel-body">

          <admin_category_tree />

          <hr />

          <form action="" method="post" class="form-inline">
            <select class="form-control" name="new-category-parent-id">
              <option value="0">Top Level Category</option>
              <category_list>
                <option value="${category_id}"><category_name /></option>
              </category_list>
            </select>
            <input type="text" name="category-name" class="form-control" placeholder="New Category Name" />
            <button class="btn btn-default" name="new-category" value="1">Add New Category</button>
          </form>

        </div>
      </div>

    </div>
  </div>
</div>

<!-- Modal -->
<div class="modal fade" id="delCat" tabindex="-1" role="dialog" aria-labelledby="myModalLabel">
  <div class="modal-dialog" role="document">
    <form action="" method="post" class="form-inline">
      <div class="modal-content">
        <div class="modal-header">
          <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
          <h4 class="modal-title" id="myModalLabel">Delete Category</h4>
        </div>
        <div class="modal-body">
          Choose a replacement category for products in this category.<br />
          <br />
          Note: products that exist in multiple categories will not be moved to the replacement category.
          Move products to:
          <input type="hidden" id="del-category-id" name="del-category-id" value="0" />
          <select class="form-control" name="del-new-category-id">
            <category_list>
              <option value="${category_id}"><category_name /></option>
            </category_list>
          </select>
        </div>
        <div class="modal-footer">
          <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
          <button type="submit" class="btn btn-danger">Delete</button>
        </div>
      </div>
    </form>
  </div>
</div>

<script>
$('.del-btn').on('click', function () { 
    $("#del-category-id").attr("value", $(this).attr("value"));
});
</script>

</apply>
