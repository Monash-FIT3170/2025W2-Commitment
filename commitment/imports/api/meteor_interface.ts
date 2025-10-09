import { Meteor } from "meteor/meteor";

/**
 * A method which properly awaits a meteor method execution, which guarantees in-order operation
 * @param method the method to call
 * @param args arguments to give the meteor method
 * @returns the value which the meteor method will return (templated for convenience)
 */
export const meteorCallAsync =
  <T = any>(method: string) =>
  (...args: any[]): Promise<T> =>
    new Promise((resolve, reject) => {
      // checks if the call is from the client, if so make sure meteor is connected
      if (Meteor.isClient && !Meteor.status().connected) reject(new Error("Server is not found"));

      // checks to see if the method is in the list of known methods
      // if (!(method in Meteor.server.method_handlers))
      //   reject(new Error(`Method not found: ${method}`))

      // calls the method, awaiting the result
      Meteor.call(method, ...args, (err: Error | null, res: T) => {
        if (err) reject(err);
        else resolve(res);
      });
    });

/**
 * a function which suppresses the error
 * @param _err the error occured in the catch
 * @returns nothing, ignore the return result
 */
export const suppressError = (_err: Error): null => null;

export const override = (text: string) => (_err: Error) => {
  throw Error(text);
};

export const overrideValue =
  <T>(value: T) =>
  (_err: Error) =>
    value;
