#!./bin/python3
from ask_sdk_core.skill_builder import SkillBuilder

from ask_sdk_core.utils import is_request_type, is_intent_name, get_slot_value
from ask_sdk_core.handler_input import HandlerInput
from ask_sdk_core.serialize import DefaultSerializer as DZ
from ask_sdk_model import Response
from ask_sdk_model.ui import SimpleCard

from operator import add, mul
from functools import reduce

sb = SkillBuilder()

# NOTE, handler_input is GLOBAL; not needed in response

def product(stack):
    return reduce(mul, stack, 1)
def summate(stack):
    return reduce(add, stack, 0)

despatch = {
    "add": summate,
    "sum": summate,
    "summate": summate,
    "multiply": product,
    "product": product
}

@sb.request_handler(can_handle_func=is_request_type("LaunchRequest"))
def launch_request_handler(handler_input: HandlerInput):
    text = "Alexa Skills Kit, calculator welcome"
    handler_input.response_builder.speak(text).set_card(
        SimpleCard("Calculator", text)).set_should_end_session(False)
    return handler_input.response_builder.response

@sb.request_handler(can_handle_func=is_intent_name("ExecuteIntent"))
def finish_intent_handler(handler_input: HandlerInput):
    text = "No calculations made"
    try:
        val = handler.attributes.session_attributes["stack"][0]
        text = f"Result is {val}"
    except:
        pass

    handler_input.response_builder.speak(text).set_card(
        SimpleCard("Calculator", text)).set_should_end_session(True)
    return handler_input.response_builder.response

@sb.request_handler(can_handle_func=is_intent_name("OperandIntent"))
def operand_intent_handler(handler_input: HandlerInput):
    val = float(get_slot_value(handler_input, "operand"))
    stack = []
    try:
        stack = DZ.deserialize(handler_input.attributes_manager.session_attributes['stack'], 'List[float]')
    except:
        pass
    stack.append(val)
    handler_input.attributes_manager.session_attributes['stack'] = DZ.serialize(stack)
    text = f"Operand intent, {val}"

    handler_input.response_builder.speak(text).set_card(
        SimpleCard("Calculator", text)).set_should_end_session(False)
    return handler_input.response_builder.response

@sb.request_handler(can_handle_func=is_intent_name("OperatorIntent"))
def operator_intent_handler(handler_input: HandlerInput):
    op = get_slot_value(handler_input, "operand")
    stack = DZ.deserialize(handler_input.attributes_manager.session_attributes["stack"], 'list[float]')
    handler_input.attributes_manager.session_attributes["stack"] = DZ.serialize([despatch[op](stack)])

    text = f"Operator intent, {op}"
    handler_input.response_builder.speak(text).set_card(
        SimpleCard("Calculator", text)).set_should_end_session(False)
    return handler_input.response_builder.response

@sb.request_handler(can_handle_func=is_intent_name("AMAZON.HelpIntent"))
def help_intent_handler(handler_input: HandlerInput):
    text = "Stack-based calculator"
    handler_input.response_builder.speak(text).set_card(
        SimpleCard("Calculator", text)).set_should_end_session(False)
    return handler_input.response_builder.response

@sb.request_handler(
    can_handle_func=lambda handler_input :
        is_intent_name("AMAZON.CancelIntent")(handler_input) or
        is_intent_name("AMAZON.StopIntent")(handler_input))
def cancel_and_stop_intent_handler(handler_input: HandlerInput):
    # type: (HandlerInput) -> Response
    speech_text = "Goodbye!"

    handler_input.response_builder.speak(speech_text).set_card(
        SimpleCard("Calculator", speech_text)).set_should_end_session(True)
    return handler_input.response_builder.response

@sb.request_handler(can_handle_func=is_request_type("SessionEndedRequest"))
def session_ended_request_handler(handler_input: HandlerInput):
    # type: (HandlerInput) -> Response
    # any cleanup logic goes here

    return handler_input.response_builder.response

@sb.exception_handler(can_handle_func=lambda i, e: True)
def all_exception_handler(handler_input: HandlerInput, exception):
    # type: (HandlerInput, Exception) -> Response
    # Log the exception in CloudWatch Logs
    print(exception)

    speech = "Sorry, I didn't get it. Can you please say it again!!"
    handler_input.response_builder.speak(speech).ask(speech)
    return handler_input.response_builder.response

lambda_handler = sb.lambda_handler()