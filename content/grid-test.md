---
comment: false
---

<style type="text/css">
.grid-container {
  display: grid;
  grid-template-columns: 50px 100px 150px;
  grid-template-rows: 100px 150px 60px;
  grid-template-areas: 'a b c'
                       'd d d'
                       'g . .';
  grid-gap: 10px 10px;
  justify-content: center;
}
.grid-item {
  border: 1px solid #ccc;
}
.grid-item3 {
  grid-area: b;
  grid-column-start: 2;
  grid-column-end: 3;
  grid-row-start: 1;
  grid-row-end: 2;
}
.grid-item5 {
  grid-column-start: 2;
  grid-column-end: 3;
  grid-row-start: 1;
  grid-row-end: 2;
  grid-area: d;
}
</style>

<div class="grid-container">
    <span class="grid-item1 grid-item">11111111111111</span>
    <span class="grid-item2 grid-item">22222</span>
    <span class="grid-item3 grid-item">33333333333333333333333</span>
    <span class="grid-item4 grid-item">44444</span>
    <span class="grid-item5 grid-item">555555555555555555555</span>
    <span class="grid-item6 grid-item">66666</span>
    <span class="grid-item7 grid-item">77777</span>
</div>
