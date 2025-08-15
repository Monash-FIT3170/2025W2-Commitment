import React, {FC} from "react";
import {Input} from "@ui/components/ui/input";
import {cx} from "class-variance-authority";
import {useFormField} from "@ui/components/ui/form";


type InputProps = React.ComponentProps<"input">;


const FormInputWithErrors = React.forwardRef<HTMLInputElement, InputProps>(
  ({className, type, ...props}, ref) => {
    const {error} = useFormField();

    return (
      <Input
        type={type}
        className={cx(className, error && "border-destructive bg-destructive-foreground")}
        ref={ref}
        {...props}
      />
    );
  }
)
FormInputWithErrors.displayName = "FormInputWithErrors"

export default FormInputWithErrors;