<apply template="_base">

<!-- Main jumbotron for a primary marketing message or call to action -->
<div class="jumbotron">
  <div class="container">
    <h1>Welcome to Pets&rsquo; Paradise! <test/></h1>
    <p>Simply the best example shop on the face of the planet!</p>
    <p><a class="btn btn-primary btn-lg" href="/browse" role="button">Browse our products</a></p>
  </div>
</div>

<!-- Example row of columns -->
<div class="container">
  <div class="row">
    <div class="col-md-6">
      <h2 style="font-family: 'Lily Script One', cursive;">About Pets&rsquo; Paradise&hellip;</h2>
      <p>This is an example online shop written in Haskell (using the <a
        href="http://snapframework.com/" target="_blank">Snap framework</a>),
      backing onto Postgresql.  Source available on
      <a href="https://github.com/rjohnsondev" target="_blank">github</a>.</p>

      <p>The idea was to try and learn enough Haskell to do something real, and
      it was intended to be just a small exercise, but it grew into something
      reasonably involved.</p>

      <p>The frontend has minimal reliance on JS (as in you can use the shop
      with it disabled), and uses good-old round trips instead of any AJAX;
      however minimal HTML and decent asset caching makes this a not-terrible
      way to go.</p>

      <p>It's pretty obvious the interface was thrown together with Bootstrap,
      but care was taken to ensure it responds nicely and scales to mobile
      screens without much fanfare.</p>

      <p>Feel free to play around in the <a href="/admin">admin</a>, the
      username is "test" and password also "test".  The database rebuilds
      itself every 10 mins or so.  Hosting is on a T2 Micro instance, so don’t
      expect too much from it.</p>

      <p>If you have any questions or feedback, hit me up on <a href="https://twitter.com/rjohnsondev" target="_blank">twitter</a>.</p>
    </div>
    <div class="col-md-6">
      <br />
      <br />
      <br />
      <p>The products listed here are actually for sale (but not through this store).  You can get them from our friends at <a href="https://www.petspyjamas.com" target="_blank">www.petspyjamas.com</a>.</p>
      <br />
      <div class="text-center">
        <a href="https://www.petspyjamas.com" target="_blank">
          <img src="/static/img/pp_logo.png" /><br />
          <img src="/static/img/pp_slogan.png" />
        </a>
      </div>

    </div>
  </div>
</div>

</apply>
