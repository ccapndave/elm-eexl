const chance = new (require("chance"))();
const fs = require("fs");
const path = require("path");
const { spawnSync } = require("child_process");
const runElm = require("@kachkaev/run-elm");

operators = [
  //"^",
  "*",
  "+",
  "<",
  "<=",
  "==",
  ">=",
  ">",
  "&&",
  "||"
];

types = [
  "Bool",
  "Int"
];

const dirName = "/tmp/eexl-generator";

function flatten(arr1) {
  return arr1.reduce((acc, val) => Array.isArray(val) ? acc.concat(flatten(val)) : acc.concat(val), []);
}

fs.existsSync(dirName) || fs.mkdirSync(dirName);
fs.writeFileSync(dirName + "/elm.json", `
  {
    "type": "application",
    "source-directories": [
        "."
    ],
    "elm-version": "0.19.0",
    "dependencies": {
        "direct": {
          "elm/core": "1.0.2",
          "elm/json": "1.1.2"
        },
        "indirect": {
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
  }
`);

const tests = [];

async function generateExpression() {
  const int = () => chance.integer({ min: 0, max: 10000 });

  const bool = () => chance.bool();

  const operator = () => chance.pickone(operators);

  const expressionList = flatten([...Array(chance.integer({ min: 2, max: 5 })).keys()].map(n =>
    [chance.pickone([int()/* , bool() */]), operator()]
  ));
  expressionList.push(chance.pickone([int()/* , bool() */]));

  const expression = expressionList.join(" ");

  // Write an Elm program to evaluate our expression
  const writeElmProgram = (expression, outputType) => {
    const program = `
module Expression${outputType} exposing (..)

output : String
output =
    expression
        ${outputType == "Int" ? "|> String.fromInt" : ""}
        ${outputType == "Bool" ? `|> (\\b -> if b == True then "True" else "False")` : ""
      }


expression: ${outputType}
expression =
    ${expression}
`;

    fs.writeFileSync(`${dirName}/Expression${outputType}.elm`, program);

    return runElm.default(`${dirName}/Expression${outputType}.elm`);
  }

  return Promise.all(types.map(type =>
    writeElmProgram(expression, type)
      .then(result => tests.push({ type, expression, compiles: true, result: result.output }))
      .catch(_ => tests.push({ type, expression, compiles: false }))
  ));

}

async function generateTest() {
  for (n = 0; n < 10; n++) {
    await generateExpression();
  }

  const program = `
module ExpressionTests exposing (suite)

import Eexl.Context as Context exposing (Context)
import Eexl.Parse exposing (Expr(..), parse)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser exposing (Problem(..))
import Test exposing (..)

suite : Test
suite =
    describe "Eexl"
      [
${tests.map(test => `
      test "${test.expression} (${test.type})" <| \\_ ->
          ${test.compiles ? `Expect.equal` : `Expect.err`}
             ${test.compiles ? `(Ok ${test.result})` : ``}
             "${test.expression}"
`).join("\n      ,")
    }
      ]
`;

  const { stdout, stderr, status } = spawnSync(path.resolve(__dirname, "..", "node_modules", ".bin", "elm-format"), ["--stdin"], { input: program });

  if (status === 0) {
    return stdout.toString();
  } else {
    console.error(stderr.toString());
    return program;
  }

}

generateTest()
  .then(testProgram => console.log(testProgram));