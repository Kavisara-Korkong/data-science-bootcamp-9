## HW 2 pizza_chatbot()

pizza_chat_2 <- function() {
    # Print the welcome message
    print("Hello, welcome ABC P!zza")
    flush.console()

    # Get customer name
    customer_name <- readline("What is your name: ")

    # Greeting customer and suggest promotion
    print(paste("Hello ,", customer_name, "!", "Promotion for this month offers a 15% discount when purchasing more than 2 Pizzas"))
    flush.console()

    # Introduce the menu & price
    # create menu and price
    pizza_price_df <- data.frame(
        pizza = c("magarita", "supreme", "veggie", "pepperoni", "seafood", "hawaiian", "bbq chicken"),
        size_S = c(199, 229, 179, 199, 229, 199, 199),
        size_M = c(249, 279, 229, 249, 279, 229, 249),
        size_L = c(299, 329, 279, 299, 329, 299, 299)
    )
    print("Here is our menu: ")
    print(pizza_price_df)
    flush.console()

    # Order loop
    orders <- data.frame()

    while (1>0) {
        # Get order for each pizza
        pizza_number <- tolower(readline("What menu number you would like to order: "))
        size <- tolower(readline("Which size (s, m, l): "))
        qty <- as.numeric(readline(paste("How many of", pizza_price_df[pizza_number, 1] ,"size", size,"do you need: ")))
        note <- readline("Do you have any special requirement: ")

        # Add order_details in orders
        order_details <- data.frame(pizza = pizza_price_df[pizza_number, 1], size = size, qty = qty, note = note)
        orders <- rbind(orders, order_details)

        # Check if customer want to order more
        pizza_2 <- readline("Do you want to order more (yes, no): ")
        if (tolower(pizza_2) == "no") {
            break
        }
    }

    # Combine order details and pizza price
    orders <- merge(orders, pizza_price_df, by = "pizza", all.x = TRUE)

    # calculate price for each row
    orders_detail <- orders
    for (order in seq_len(nrow(orders))) {
        if (orders_detail[order, "size"] == "s") {
            pizza_price <- orders[order, "size_S"]} else if (orders_detail[order, "size"] == "m") {
            pizza_price <- orders[order, "size_M"]} else if (orders_detail[order, "size"] == "l") {
            pizza_price <- orders[order, "size_L"]}

        sub_total <- orders_detail[order, "qty"] * (pizza_price)

        orders_detail[order, "sub_total"] <- sub_total
    }

    # summary order detail
    print("Your Order Summary")
    print(orders_detail[, c("pizza","size","qty","sub_total","note")])
    flush.console()

    # calculate total price
    if (sum(orders_detail$qty) >= 3) {
        total_price = 0.85 * sum(orders_detail$sub_total)
        discount = 0.15 * sum(orders_detail$sub_total)
        print(paste0("You have a discount of ", discount, "฿"))
        flush.console()

    } else {
        total_price = sum(orders_detail$sub_total)
    }
    total_price = sum(orders_detail$sub_total)
    print(paste0("Your Total Bill is ", total_price, "฿"))
    flush.console()


    # confirm order
    confirm_order <- readline("Would you like to confirm order (yes, no): ")
    if (confirm_order == "yes") {
        phone_number <- readline("What is your phone number: ")
        delivery_method <- readline("Choose 1 if you want pick up at store, Choose 2 if you want delivery service (50฿ for delivery fee)")
            if (delivery_method == 1) {
                print("Your order will be ready in 30 minutes, you can pick at the store")
            } else if (delivery_method == 2) {
                address <- readline("What is your address: ")
                print("Your order will be arrive in 45 minutes")
                print("The delivery fee is 50฿")
                print(paste0("Your Total Bill is ", total_price + 50, "฿"))
            }
        print("Thank you!")
    } else if (confirm_order == "no") {
        print("Let's order again!")
        pizza_chat_2()
    }
}
