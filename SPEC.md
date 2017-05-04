Imagine the backend API of a typical coupon module that would be getting used in shopping cart applications. 
End-customers would typically be discovering these coupons via some promotion or deal sites (like CouponRaja or CouponDunia).
Entering this coupon during the checkout process would give the customer some discount.

* Design & implement a REST API to create and read coupons.

* We should be able to define the applicable discount based on
-- Flat discount amount per order
-- Flat discount amount per item
-- Percentage discount on total order amount

* We should be able to restrict coupon usage based on the following
-- All products or list of products
-- Orders placed between given start/end dates
-- Orders with total amount higher than given value
-- Limit coupon usage per customer (tracked by customer email), i.e. customer is allowed to use a coupon on 'N' times
-- Limit coupon usage per product, i.e. coupon is allowed to be used 'N' number of times for a given product(s)
-- Limit coupon usage, i.e. coupon is allowed to be used 'N' number of times across all customers and products

* Design & implement a REST API to validate coupon usage and respond with the final discount amount. 
The API can accept a list of items/products added to the shopping card,
and the coupon that the customer is trying to apply. 
The API should respond with whether the coupon is applicable or not, and discount amount.