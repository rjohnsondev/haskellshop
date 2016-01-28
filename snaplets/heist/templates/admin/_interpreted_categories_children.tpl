<category>
  <div class="cat">
    <span class="text-info pull-right">
      <button
        style="${category_hide_delete_block}"
        value="${id}"
        type="button"
        class="glyphicon glyphicon-remove btn btn-danger btn-xs del-btn"
        data-toggle="modal"
        data-target="#delCat"
        ></button>
    </span>
    <form action="" method="post" class="text-info pull-right">
      <button
        name="category-move-up"
        style="margin-right: 5px;"
        value="${id}"
        type="submit"
        class="glyphicon glyphicon-chevron-up btn btn-primary btn-xs"
        ></button>
    </form>
    <form action="" method="post" class="text-info pull-right">
      <button
        name="category-move-down"
        style="margin-right: 2px;"
        value="${id}"
        type="submit"
        class="glyphicon glyphicon-chevron-down btn btn-primary btn-xs"
        ></button>
    </form>
    <name />
    <children />
  </div>
</category>
