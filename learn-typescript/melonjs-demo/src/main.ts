import * as me from "melonjs";

me.device.onReady(function () {
  // Initialize the display canvas once the device/browser is ready
  if (!me.video.init(1218, 562, { parent: "screen", scale: "auto" })) {
    alert("Your browser does not support HTML5 canvas.");
    return;
  }

  // Add a gray background
  me.game.world.addChild(new me.ColorLayer("background", "#202020"));

  // Add a font text display object
  me.game.world.addChild(
    new me.Text(609, 281, {
      font: "Arial",
      size: 160,
      fillStyle: "#FFFFFF",
      textBaseline: "middle",
      textAlign: "center",
      text: "Hello World!"
    })
  );
});
