import React, { FC } from "react";
import { Input } from "@ui/components/ui/input";
import { cx } from "class-variance-authority";
import { useFormField } from "@ui/components/ui/form";

type InputProps = React.ComponentProps<"input">;
export interface FormInputWithErrors extends InputProps {
  hasError?: boolean;
}

const FormInputWithErrors: FC<FormInputWithErrors> = (props) => {
  const { error } = useFormField();

  return (
    <Input
      {...props}
      className={cx(
        props.className,
        error && "dark:text-black border-destructive bg-destructive-foreground"
      )}
    />
  );
};

export default FormInputWithErrors;
