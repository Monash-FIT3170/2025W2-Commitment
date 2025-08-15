import React from 'react';
import { shallow } from 'enzyme';
import { assert, expect } from 'chai';
import LoginWidget from "@ui/components/widgets/login/LoginWidget.tsx";
import LoginForm from "@ui/components/widgets/login/LoginForm/LoginForm";

describe('LoginWidget', () => {
  it('should render', () => {
    const item = shallow(<LoginWidget />);
    expect(item.find(LoginForm), "should have a LoginForm").to.have.lengthOf(1);
  });
  it('sign up should render', () => {
    const item = shallow(<LoginWidget defaultTab="signup"/>);
    expect(item.find(LoginForm), "should have a LoginForm").to.have.lengthOf(1);
  });
});