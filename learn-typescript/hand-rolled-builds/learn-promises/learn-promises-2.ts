function task1(): Promise<string> {
  return new Promise<string>((resolve, reject) => {
    console.log(`${Date.now() / 1000} - Starting task 1...`);
    setTimeout(() => {
      console.log(`${Date.now() / 1000} - task 1 done.`);
      resolve("task 1 result");
    }, 5000);
  });
}

function task2(): Promise<string> {
  return new Promise<string>((resolve, reject) => {
    console.log(`${Date.now() / 1000} - Starting task 2...`);
    setTimeout(() => {
      console.log(`${Date.now() / 1000} - task 2 done.`);
      resolve("task 2 result");
    }, 2000);
  });
}

function task3(): Promise<string> {
  return new Promise<string>((resolve, reject) => {
    console.log(`${Date.now() / 1000} - Starting task 3...`);
    setTimeout(() => {
      console.log(`${Date.now() / 1000} - task 3 done.`);
      resolve("task 3 result");
    }, 1000);
  });
}

async function main_1() {
  const t1 = await task1();
  console.log(t1);

  const t2 = await task2();
  console.log(t2);

  const t3 = await task3();
  console.log(t3);
}

async function main() {
  const promise1 = task1();
  const promise2 = task2();
  const promise3 = task3();
  try {
    const results = await Promise.all([promise1, promise2, promise3]);
    results.forEach((result) => console.log(result));
  } catch (err) {
    // Even if one of the promises fails, the exception will be thrown without
    // waiting for the other tasks.
    console.error(err);
  }
}

main();
