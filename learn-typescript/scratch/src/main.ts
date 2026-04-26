const previews = [
  "placeholder-0",
  "placeholder-1",
  "placeholder-2",
  "placeholder-3",
  "placeholder-4",
  "placeholder-5"
];

function onScrollEnd() {
  console.log(`(${new Date().valueOf()}) - onScrollEnd()`);
  const previewsDiv = document.getElementById("previews")!;

  // Complete the scroll of any partially scrolled preview
  // TODO: Explore if Intersection Observer API will be a better way
  let visiblePreview: HTMLElement;
  let element = Array.from(previewsDiv.children).find(
    (child) => child.getBoundingClientRect().top >= 0
  );
  if (!element) {
    // If the last element is visible, then the top scrolls to -4
    console.debug("element was null, setting visible to last element");
    visiblePreview = previewsDiv.lastElementChild! as HTMLElement;
  } else {
    console.debug("found visible element");
    if (element.getBoundingClientRect().top < window.innerHeight / 2) {
      // This element is near the top, make this visible
      visiblePreview = element as HTMLElement;
    } else {
      // This element is closer to the bottom, make the previous element visible
      visiblePreview = element.previousElementSibling! as HTMLElement;
    }
  }

  const top = Math.round(visiblePreview.getBoundingClientRect().top);
  if (top !== 0) {
    console.debug(
      `Scrolling ${visiblePreview.id} into view (${new Date().valueOf()})`
    );
    visiblePreview.scrollIntoView({ behavior: "smooth" });
  } else {
    console.debug(`Element ${visiblePreview.id} is already on top`);
  }
}

const previewsDiv = document.getElementById("previews")! as HTMLElement;
let timeoutHandle: number = 0;
previewsDiv.onscroll = (e: Event) => {
  clearTimeout(timeoutHandle);
  timeoutHandle = setTimeout(onScrollEnd, 3000);
};
