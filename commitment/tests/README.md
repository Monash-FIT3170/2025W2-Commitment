# Tests

To run tests, run this command from the repo root

```sh
cd ..
meteor npm run test
```

## Adding new tests

To write new test, add a file to this directory (or a subdirectory of this directory), named in the format `<thing to test>.test.ts(x)`.

Then, an import to [`main.ts`](./main.ts):

```ts
// Add your tests here
import '<thing to test>.test.ts(x)';
```

### React component tests

```tsx
import { Factory } from 'meteor/dburles:factory';
import React from 'react';
import { shallow } from 'enzyme';
import { assert, expect } from 'chai';

// Import your component to test:
import MyWidget from "@ui/components/widgets/.../MyWidget.tsx";

// You can define some default data to use in your tests like this:
// Inlcuding a MongoDB Collection is optional, but it allows you to query the db directly in your tests
Factory.define('test-data', SomeMongoCollection, {
  text: 'Default text',
  checked: true
});


// Describe with your component name
describe('LoginWidget', () => {
  // Add these for each unit test:
  it('should render', () => {
    // This code block is a single test case
    
    // You can get data with some overrides of the default values like this:
    const testData = Factory.build('test-data', { text: 'testing', checked: false });
    
    // Render an instance of your component
    const item = shallow(<MyWidget {...testData}/>);
    
    // Make assertions
    assert.isTrue(item.hasClass('h-screen'));
  });

  // And if you want to test interactions with the component:
  it('test simulated interaction', () => {
    
    // We can use sinon to fake stuff. See https://sinonjs.org/ for more ways to fake.
    // Also see https://enzymejs.github.io/enzyme/ for more examples.
    sinon.stub(setCheckedStatus, 'call');
    
    // You can get data with some overrides of the default values like this:
    const testData = Factory.build('test-data', { text: 'testing', checked: false });
    const item = shallow(<MyWidget {...testData}/>);

    item.find('input[type="checkbox"]').simulate('change', {
      target: { checked: true },
    });

    sinon.assert.calledWith(setCheckedStatus.call, {
      todoId: todo._id,
      newCheckedStatus: true,
    });

    setCheckedStatus.call.restore();
  });

  // You can add more it('name', () => {/* test code */}) calls below
});
```