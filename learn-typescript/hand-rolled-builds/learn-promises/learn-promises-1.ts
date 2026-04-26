type Cookie = {
  flavor: string;
  calories: number;
};

async function bake(): Promise<Cookie> {
  return new Promise<Cookie>((resolve, reject) => {
    console.log("bake::executor: starting");
    // baking the cookie takes some time, it may or may not come out well
    const success = Math.random() > 0.5;
    if (success) {
      const cookie = {
        flavor: "Chocolate Chip",
        calories: 200
      };
      console.log("bake::executor: resolving");
      resolve(cookie);
    } else {
      console.log("bake::executor: rejecting");
      reject(new Error("Bad cookie!"));
    }
  });
}

async function learnPromises() {
  try {
    const resp = await bake();
    console.log("Main: promise resolved");
    console.log(typeof resp, resp);
  } catch (err) {
    console.log("Main: promise rejected");
    console.error(typeof err, err.name, err.message);
  }
}

function learnThenables() {
  console.log("Main: calling bake");
  bake()
    .then((cookie) => console.log(`Got cookie: ${cookie}`))
    .catch((err) => console.error(`Got error: ${err}`));
}

// learnPromises();
learnThenables();
