import React, {FC} from "react";
import {Button} from "@ui/components/ui/button";
import { z } from "zod";
import {zodResolver} from "@hookform/resolvers/zod";
import {useForm} from "react-hook-form";
import {Form, FormControl, FormField, FormItem, FormLabel, FormMessage} from "@ui/components/ui/form";
import {Checkbox} from "@ui/components/ui/checkbox";
import FormInputWithErrors from "../../../shared/FormInputWithErrors";



export interface LoginFormProps {
  className?: string
}


const formSchema = z.object({
  email: z.string().email(),
  password: z.string().min(8),
  rememberMe: z.boolean()
})


const LoginForm: FC<LoginFormProps> = (props) => {

  const form = useForm<z.infer<typeof formSchema>>({
    resolver: zodResolver(formSchema),
    defaultValues: {
      email: "",
      password: "",
      rememberMe: false
    },
  });

  function onSubmit(values: z.infer<typeof formSchema>) {
    // TODO: Replace with actual functionality
    console.log(values);
  }

  return (
    <Form {...form}>
      <form onSubmit={form.handleSubmit(onSubmit)} className={`flex flex-col gap-2 ${props.className ?? ""}`}>
        <FormField
          control={form.control}
          name="email"
          render={({ field }) => (
            <FormItem>
              <FormControl>
                <FormInputWithErrors placeholder="Email" {...field} type="email"/>
              </FormControl>
              <FormMessage displayWithoutError={true} className="transition-all" noErrorClassName="h-0 opacity-0" errorClassName="h-5"/>
            </FormItem>
          )}
        />

        <FormField
          control={form.control}
          name="password"
          render={({ field }) => (
            <FormItem>
              <FormControl>
                <FormInputWithErrors placeholder="Password" type="password" {...field}/>
              </FormControl>
              <FormMessage displayWithoutError={true} className="transition-all ease-out" noErrorClassName="h-0 opacity-0" errorClassName="h-5"/>
            </FormItem>
          )}
        />

        <div className="inline-flex flex-col justify-center h-9">
          <FormField
            control={form.control}
            name="rememberMe"
            render={({ field }) => (
              <FormItem>

                <div className="flex items-center space-x-2 ml-3">
                  <FormControl>
                    <Checkbox checked={field.value} onCheckedChange={field.onChange}/>
                  </FormControl>
                  <div className="space-y-1 leading-none align-middle inline-flex flex-col content-center">
                    <FormLabel>
                      Remember me?
                    </FormLabel>
                  </div>
                </div>
              </FormItem>
            )}
          />
        </div>

        <Button type="submit" className="w-full">Submit</Button>
      </form>
    </Form>
  );
}

export default LoginForm;