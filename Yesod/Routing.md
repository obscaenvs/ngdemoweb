Concerning type-safe routing in AngularJS 
=========================================

__tl;dr__ Routing in AngularJS is somewhat complicated.

- We have one type of syntax for client-side definitions of the
routes, and must name the parameters to make the arguments accessible
in the controllers.

- The (probably) most used type of link in AngularJS is the link
within the client, i.e. a link to another view within the same client
with some arguments passed along. For instance, we can—after client side
interpretation—have a link to a view "PersonView" along the lines of

  ```html
  <a ng-href="#/PersonView/Age/39/Name/Assurbanipal">Go Assurbanipal</a>
  ```
(where my intention is that the "39" and "Assurbanipal" are arguments
to "PersonView"). These arguments are mostly produced by AngularJS
itself (by e.g. an ng-repeat or a similar mechanism), which introduces
an issue: we have to provide the variable name in the model (i.e.
naming some variable set on a $scope, for some controller) that the
argument is bound to, akin to:

  ```html
  <a ng-href='#/PersonView/Age/{{person.age}}/Name/{{person.name}}'>Go {{person.name}}</a>
  ```
- When you want to use e.g. a button to switch views, you have to use
yet another type of syntax to provide a link of sorts: this time as an
argument to a JavaScript function on the current scope (or the root
scope, I guess), since buttons have yet to be scripted to make this
functionality possible. Let's say this function is defined on the
current controller, and looks like:

  ```javascript
  $scope.go = function ( p ) {
    $location.path( p );
  };
  ```
Then we need to call this function with code like so (no highlight
here because of mixed syntax):

  ```
  <button data-ng-click="go('/PersonView/Age/' + person.age + '/Name/' + person.name);">
  Go {{person.name}}!
  </button>
  ```
Inline scripts like this do not work with handlebar interpolation of
the controller variables, so we have to use pure JavaScript syntax.

Ideally, we would like to support this using as little Template Haskell as possible.