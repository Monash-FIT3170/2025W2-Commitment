import {FormMessage} from "@ui/components/ui/form";
import React from "react";

const LoginFormErrorMessage = () => (
  <FormMessage displayWithoutError={true} className="transition-all" noErrorClassName="max-h-0 opacity-0" errorClassName="max-h-10 opacity-100"/>
);

export default LoginFormErrorMessage;
