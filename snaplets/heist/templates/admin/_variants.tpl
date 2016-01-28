<apply template="_base">
<!-- Example row of columns -->
<div class="container" style="margin-top: 20px;">
  <ol class="breadcrumb">
    <li class="active">Product Options</li>
  </ol>
</div>

<div class="container">
  <div class="row">
    <div class="col-md-12">
      <apply template="_err"/>

      <variants>
        <div class="panel panel-default">
          <div class="panel-heading">
            <variant_name />
            <form action="" method="post" class="text-danger pull-right">
              <button name="del-variant-id" style="${variant_hide_delete_block}" value="${variant_id}" type="submit" class="glyphicon glyphicon-remove btn btn-danger btn-xs"></button>
            </form>
          </div>
          <div class="panel-body">
            <ul class="list-group">
              <variant_options>
              <li class="list-group-item">
                <variant_option_name />
                <form action="" method="post" class="text-danger pull-right kill-btn-form" data-name="${variant_option_name}">
                  <variant_option_del_btn>
                    <button name="del-variant-option-id" type="submit" class="glyphicon glyphicon-remove btn btn-danger btn-xs"></button>
                  </variant_option_del_btn>
                </form>
              </li>
              </variant_options>
            </ul>
            <form action="" method="post" class="form-inline">
              <input type="text" name="variant-option-${variant_id}" class="form-control" placeholder="New Option" />
              <button class="btn btn-default" name="new-variant-option" value="${variant_id}">Add New Variant Option</button>
            </form>
          </div>
        </div>
      </variants>


      <form action="" method="post" class="form-inline">
        <input type="text" name="variant-name" class="form-control" placeholder="New Variant Name" />
        <button class="btn btn-default" name="new-variant" value="1">Add New Variant</button>
      </form>

    </div>
  </div>
</div>

<script>
$(".kill-btn-form").each(function (_, frm) {
    $(frm).on("submit", function() {
        return confirm('Are you sure you want to remove option "' + $(frm).data("name") + '"');
    });
});
</script>
</apply>
