function logResponse(resp: Response) {
  console.log("\n-----------------");
  console.log(resp);
  console.log("-----------------\n");
}

async function learnFetch(url: string) {
  console.log(`\nFetching: ${url}`);

  try {
    const resp = await fetch(url);

    if (!resp.ok) {
      console.error("Got not-ok response:");
      const details = await resp.json();
      console.error(details);
      logResponse(resp);
      return;
    }

    // Will work even if the data is just a string
    const data = await resp.json();
    console.log(`Fetched data of type ${typeof data}`);
    console.log(data);
    logResponse(resp);
  } catch (err) {
    if (err instanceof Error) {
      console.error(`Name: ${err.name}`);
      console.error(`Message: ${err.message}`);
      console.error("---Details---");
      console.error(err.cause);
    } else {
      console.error("Caught generic error object");
      console.error(err);
    }
  }
}

const url = process.argv.slice(2, 3)[0];
learnFetch(url);
