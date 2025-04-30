import React, {FC} from "react";
import {Button} from "@ui/components/ui/button";
import {Input} from "@ui/components/ui/input";
import { z } from "zod";
import {zodResolver} from "@hookform/resolvers/zod";
import {useForm} from "react-hook-form";
import {Form, FormControl, FormField, FormItem, FormLabel, FormMessage} from "@ui/components/ui/form";
import {Checkbox} from "@ui/components/ui/checkbox";

export interface LoginFormProps {

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

      <form onSubmit={form.handleSubmit(onSubmit)} className="flex flex-col gap-2">
        <FormField
          control={form.control}
          name="email"
          render={({ field }) => (
            <FormItem>
              <FormControl>
                <Input placeholder="Email" {...field} type="email"/>
              </FormControl>
              <FormMessage />
            </FormItem>
          )}
        />

        <FormField
          control={form.control}
          name="password"
          render={({ field }) => (
            <FormItem>
              <FormControl>
                <Input placeholder="Password" {...field} type="password"/>
              </FormControl>
              <FormMessage />
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

/*
    <Card className="w-80">
      <CardHeader>
        <CardTitle>Log in</CardTitle>
        <CardDescription>
          Make changes to your account here. Click save when you're done.
        </CardDescription>
      </CardHeader>
      <CardContent className="space-y-2">
        <Input id="email" defaultValue="" placeholder="Email" aria-label="Email" type="email"/>
        <Input id="password" defaultValue="" placeholder="Password" aria-label="Password" type="password"/>
      </CardContent>
      <CardFooter>
        <Button className="w-full">Save changes</Button>
      </CardFooter>
    </Card>
 */

export default LoginForm;