#!/usr/bin/env python3
"""Test script for Emacs MCP tools.

This script tests the actual MCP server running in Emacs to verify all tools are working properly.
Run this from your emacs config directory with: python test_emacs_mcp_tools.py

Prerequisites:
- The emacs-mcp-stdio.sh script must exist and be executable
- MCP server settings must be properly configured
"""

import json
import subprocess
import sys
import tempfile
import os
import logging
from typing import Dict, Any, Optional


class MCPClient:
    """Simple MCP client for testing purposes."""
    
    def __init__(self, script_path: str = "/Users/luis.moneda/.emacs.d/emacs-mcp-stdio.sh"):
        self.script_path = script_path
        self._validate_script_path()
    
    def _validate_script_path(self):
        """Validate that the MCP script exists and is executable."""
        if not os.path.exists(self.script_path):
            raise FileNotFoundError(f"MCP script not found at: {self.script_path}")
        if not os.access(self.script_path, os.X_OK):
            raise PermissionError(f"MCP script is not executable: {self.script_path}")
    
    def call_tool(self, name: str, arguments: Dict[str, Any]) -> Any:
        """Call an MCP tool via stdio transport."""
        try:
            # Create JSON-RPC request
            request = {
                "jsonrpc": "2.0",
                "id": 1,
                "method": "tools/call",
                "params": {
                    "name": name,
                    "arguments": arguments
                }
            }
            
            request_json = json.dumps(request)
            print(f"  → Calling {name} with: {arguments}")
            
            # Start the subprocess
            process = subprocess.Popen(
                [self.script_path],
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                bufsize=0
            )
            
            # Send request and get response
            stdout, stderr = process.communicate(input=request_json + "\n", timeout=30)
            
            if stderr:
                print(f"  ⚠ Stderr: {stderr.strip()}")
            
            if not stdout.strip():
                return {"error": "Empty response from MCP server"}
            
            # Parse JSON response
            try:
                response = json.loads(stdout.strip())
            except json.JSONDecodeError as e:
                return {"error": f"Invalid JSON response: {stdout[:200]}..."}
            
            # Check for JSON-RPC error
            if "error" in response:
                return {"error": response["error"]}
            
            # Extract result
            result = response.get("result", {})
            return {"success": True, "result": result, "raw_response": response}
            
        except subprocess.TimeoutExpired:
            return {"error": "Tool call timed out"}
        except Exception as e:
            return {"error": f"Unexpected error: {str(e)}"}


class EmacsMCPTester:
    """Test class for Emacs MCP tools."""
    
    def __init__(self):
        self.client = MCPClient()
        self.test_results = {}
        self.failed_tests = []
        
    def call_mcp_tool(self, tool_name: str, params: Dict[str, Any]) -> Dict[str, Any]:
        """Call an MCP tool and return the result."""
        return self.client.call_tool(tool_name, params)
    
    def _extract_tool_result(self, result: Dict[str, Any]) -> Any:
        """Extract the actual tool result from MCP response format."""
        tool_result = result.get("result", {})
        
        # Handle MCP content array format: {"content": [{"type": "text", "text": "..."}]}
        if isinstance(tool_result, dict) and "content" in tool_result:
            content_array = tool_result.get("content", [])
            if content_array and len(content_array) > 0:
                first_content = content_array[0]
                if isinstance(first_content, dict) and "text" in first_content:
                    # Extract the JSON string from the text field
                    tool_result = first_content["text"]
        
        # Handle case where result is a JSON string
        if isinstance(tool_result, str):
            try:
                tool_result = json.loads(tool_result)
            except json.JSONDecodeError:
                pass
        
        return tool_result
    
    def validate_tool_result(self, tool_name: str, result: Dict[str, Any]) -> tuple[bool, str]:
        """Validate that a tool result indicates actual success, not just MCP communication success."""
        if not result.get("success"):
            return False, f"MCP error: {result.get('error', 'Unknown error')}"
        
        tool_result = self._extract_tool_result(result)
        
        # Check for error strings
        if isinstance(tool_result, str) and "Error:" in tool_result:
            return False, f"Tool error: {tool_result}"
        
        # Check for error indicators in the result
        if isinstance(tool_result, dict):
            if tool_result.get("success") is False:
                return False, f"Tool reported failure: {tool_result.get('error', 'Unknown error')}"
            
            # Tool-specific validations
            if tool_name == "org-roam-search-nodes-by-title":
                if "search_term" not in tool_result or "nodes" not in tool_result:
                    return False, "Missing required fields (search_term, nodes)"
                    
            elif tool_name == "get-available-agenda-locations":
                if "file" not in tool_result or "available_locations" not in tool_result:
                    return False, "Missing required fields (file, available_locations)"
                    
            elif tool_name in ["add-todo-simple", "add-todo-with-refile"]:
                if "content" not in tool_result or "location" not in tool_result:
                    return False, "Missing required fields (content, location)"
                if not tool_result.get("success", False):
                    return False, "Tool did not report success"
                    
            elif tool_name in ["execute_src_block", "execute_buffer"]:
                if "file_path" not in tool_result:
                    return False, "Missing required field (file_path)"
                    
            elif tool_name == "org-roam-retrieve-node-by-id":
                if "id" not in tool_result or "content" not in tool_result:
                    return False, "Missing required fields (id, content)"
                    
            elif tool_name == "org-roam-get-backlinks":
                if "target_id" not in tool_result or "backlinks" not in tool_result:
                    return False, "Missing required fields (target_id, backlinks)"
        
        # If we get here, the tool result looks valid
        return True, "Tool executed successfully"

    def test_org_roam_tools(self):
        """Test org-roam related tools."""
        print("\n=== Testing Org-Roam Tools ===")
        
        # Test org-roam-search-nodes-by-title
        print("Testing org-roam-search-nodes-by-title...")
        result = self.call_mcp_tool("org-roam-search-nodes-by-title", {
            "title": "emacs",
            "limit": 5
        })
        self.test_results["org-roam-search-nodes-by-title"] = result
        is_valid, message = self.validate_tool_result("org-roam-search-nodes-by-title", result)
        if is_valid:
            print("✓ org-roam-search-nodes-by-title: PASSED")
            # Extract and display the parsed result
            parsed_result = self._extract_tool_result(result)
            print(f"  Result: {json.dumps(parsed_result, indent=2)[:500]}...")
        else:
            print(f"✗ org-roam-search-nodes-by-title: FAILED - {message}")
            print(f"  Raw result: {json.dumps(result.get('result', {}), indent=2)[:300]}...")
            self.failed_tests.append("org-roam-search-nodes-by-title")
        
        # Test get-available-agenda-locations
        print("Testing get-available-agenda-locations...")
        result = self.call_mcp_tool("get-available-agenda-locations", {})
        self.test_results["get-available-agenda-locations"] = result
        is_valid, message = self.validate_tool_result("get-available-agenda-locations", result)
        if is_valid:
            print("✓ get-available-agenda-locations: PASSED")
            parsed_result = self._extract_tool_result(result)
            print(f"  Result: {json.dumps(parsed_result, indent=2)[:500]}...")
        else:
            print(f"✗ get-available-agenda-locations: FAILED - {message}")
            print(f"  Raw result: {json.dumps(result.get('result', {}), indent=2)[:300]}...")
            self.failed_tests.append("get-available-agenda-locations")

    def test_org_agenda_tools(self):
        """Test org-agenda related tools."""
        print("\n=== Testing Org-Agenda Tools ===")
        
        # Test add-todo-simple
        print("Testing add-todo-simple...")
        test_content = f"Test TODO item from MCP test script - {self.get_timestamp()}"
        result = self.call_mcp_tool("add-todo-simple", {
            "content": test_content
        })
        self.test_results["add-todo-simple"] = result
        is_valid, message = self.validate_tool_result("add-todo-simple", result)
        if is_valid:
            print("✓ add-todo-simple: PASSED")
            parsed_result = self._extract_tool_result(result)
            print(f"  Result: {json.dumps(parsed_result, indent=2)[:500]}...")
        else:
            print(f"✗ add-todo-simple: FAILED - {message}")
            print(f"  Raw result: {json.dumps(result.get('result', {}), indent=2)[:300]}...")
            self.failed_tests.append("add-todo-simple")
        
        # Test add-todo-with-refile
        print("Testing add-todo-with-refile...")
        test_content_refile = f"Test TODO with refile from MCP test script - {self.get_timestamp()}"
        result = self.call_mcp_tool("add-todo-with-refile", {
            "content": test_content_refile,
            "refile_target": "Life/Misc"  # Safe target that should always exist
        })
        self.test_results["add-todo-with-refile"] = result
        is_valid, message = self.validate_tool_result("add-todo-with-refile", result)
        if is_valid:
            print("✓ add-todo-with-refile: PASSED")
            parsed_result = self._extract_tool_result(result)
            print(f"  Result: {json.dumps(parsed_result, indent=2)[:500]}...")
        else:
            print(f"✗ add-todo-with-refile: FAILED - {message}")
            print(f"  Raw result: {json.dumps(result.get('result', {}), indent=2)[:300]}...")
            self.failed_tests.append("add-todo-with-refile")

    def test_org_babel_tools(self):
        """Test org-babel related tools."""
        print("\n=== Testing Org-Babel Tools ===")
        
        # Create a temporary org file for testing
        with tempfile.NamedTemporaryFile(mode='w', suffix='.org', delete=False) as f:
            f.write("""#+TITLE: Test Org Babel File

* Test Section

#+NAME: test-block
#+BEGIN_SRC python
print("Hello from MCP test!")
result = 2 + 2
print(f"2 + 2 = {result}")
#+END_SRC

#+BEGIN_SRC python
# Another test block
import datetime
print(f"Current time: {datetime.datetime.now()}")
#+END_SRC

""")
            test_file_path = f.name
        
        try:
            # Test execute_src_block (correct name from MCP registration)
            print("Testing execute_src_block...")
            result = self.call_mcp_tool("execute_src_block", {
                "file_path": test_file_path,
                "block_name": "test-block"
            })
            self.test_results["execute_src_block"] = result
            is_valid, message = self.validate_tool_result("execute_src_block", result)
            if is_valid:
                print("✓ execute_src_block: PASSED")
                parsed_result = self._extract_tool_result(result)
                print(f"  Result: {json.dumps(parsed_result, indent=2)[:500]}...")
            else:
                print(f"✗ execute_src_block: FAILED - {message}")
                print(f"  Raw result: {json.dumps(result.get('result', {}), indent=2)[:300]}...")
                self.failed_tests.append("execute_src_block")
            
            # Test execute_buffer (correct name from MCP registration)
            print("Testing execute_buffer...")
            result = self.call_mcp_tool("execute_buffer", {
                "file_path": test_file_path
            })
            self.test_results["execute_buffer"] = result
            is_valid, message = self.validate_tool_result("execute_buffer", result)
            if is_valid:
                print("✓ execute_buffer: PASSED")
                parsed_result = self._extract_tool_result(result)
                print(f"  Result: {json.dumps(parsed_result, indent=2)[:500]}...")
            else:
                print(f"✗ execute_buffer: FAILED - {message}")
                print(f"  Raw result: {json.dumps(result.get('result', {}), indent=2)[:300]}...")
                self.failed_tests.append("execute_buffer")
                
        finally:
            # Clean up temporary file
            try:
                os.unlink(test_file_path)
            except:
                pass

    def test_org_roam_specific_tools(self):
        """Test org-roam tools that need specific IDs."""
        print("\n=== Testing Org-Roam Specific Tools ===")
        
        # First, try to find some nodes to test with
        search_result = self.call_mcp_tool("org-roam-search-nodes-by-title", {
            "title": "emacs",
            "limit": 1
        })
        
        if search_result.get("success") and search_result.get("result"):
            result_data = search_result["result"]
            if isinstance(result_data, str):
                try:
                    result_data = json.loads(result_data)
                except:
                    pass
            
            nodes = result_data.get("nodes", []) if isinstance(result_data, dict) else []
            
            if nodes:
                test_node_id = nodes[0].get("id")
                if test_node_id:
                    # Test org-roam-retrieve-node-by-id
                    print(f"Testing org-roam-retrieve-node-by-id with ID: {test_node_id}")
                    result = self.call_mcp_tool("org-roam-retrieve-node-by-id", {
                        "roam_id": test_node_id
                    })
                    self.test_results["org-roam-retrieve-node-by-id"] = result
                    is_valid, message = self.validate_tool_result("org-roam-retrieve-node-by-id", result)
                    if is_valid:
                        print("✓ org-roam-retrieve-node-by-id: PASSED")
                        parsed_result = self._extract_tool_result(result)
                        print(f"  Result: {json.dumps(parsed_result, indent=2)[:500]}...")
                    else:
                        print(f"✗ org-roam-retrieve-node-by-id: FAILED - {message}")
                        print(f"  Raw result: {json.dumps(result.get('result', {}), indent=2)[:300]}...")
                        self.failed_tests.append("org-roam-retrieve-node-by-id")
                    
                    # Test org-roam-get-backlinks
                    print(f"Testing org-roam-get-backlinks with ID: {test_node_id}")
                    result = self.call_mcp_tool("org-roam-get-backlinks", {
                        "roam_id": test_node_id,
                        "limit": 5
                    })
                    self.test_results["org-roam-get-backlinks"] = result
                    is_valid, message = self.validate_tool_result("org-roam-get-backlinks", result)
                    if is_valid:
                        print("✓ org-roam-get-backlinks: PASSED")
                        parsed_result = self._extract_tool_result(result)
                        print(f"  Result: {json.dumps(parsed_result, indent=2)[:500]}...")
                    else:
                        print(f"✗ org-roam-get-backlinks: FAILED - {message}")
                        print(f"  Raw result: {json.dumps(result.get('result', {}), indent=2)[:300]}...")
                        self.failed_tests.append("org-roam-get-backlinks")
                else:
                    print("⚠ No node ID found in search results, skipping ID-based tests")
            else:
                print("⚠ No nodes found in search, skipping ID-based tests")
        else:
            print("⚠ Could not search for nodes, skipping ID-based tests")

    def get_timestamp(self) -> str:
        """Get current timestamp for test items."""
        from datetime import datetime
        return datetime.now().strftime("%Y-%m-%d %H:%M:%S")

    def run_all_tests(self):
        """Run all tests and print summary."""
        print("Starting Emacs MCP Tools Test Suite")
        print("=" * 50)
        
        try:
            self.test_org_roam_tools()
            self.test_org_agenda_tools()
            self.test_org_babel_tools()
            self.test_org_roam_specific_tools()
            
            print("\n" + "=" * 50)
            print("TEST SUMMARY")
            print("=" * 50)
            
            total_tests = len(self.test_results)
            passed_tests = total_tests - len(self.failed_tests)
            
            print(f"Total tests run: {total_tests}")
            print(f"Passed: {passed_tests}")
            print(f"Failed: {len(self.failed_tests)}")
            
            if self.failed_tests:
                print(f"\nFailed tests: {', '.join(self.failed_tests)}")
                print("\nDetailed error information:")
                for test_name in self.failed_tests:
                    result = self.test_results.get(test_name, {})
                    print(f"\n{test_name}:")
                    if "error" in result:
                        print(f"  Error: {result['error']}")
                    if "stderr" in result:
                        print(f"  Stderr: {result['stderr']}")
                    if "stdout" in result:
                        print(f"  Stdout: {result['stdout']}")
            else:
                print("\n🎉 All tests passed!")
                
            return len(self.failed_tests) == 0
            
        except Exception as e:
            print(f"\nUnexpected error during testing: {str(e)}")
            import traceback
            traceback.print_exc()
            return False


def main():
    """Main function to run the tests."""
    tester = EmacsMCPTester()
    success = tester.run_all_tests()
    
    if success:
        print("\n✅ All MCP tools are working correctly!")
        sys.exit(0)
    else:
        print("\n❌ Some MCP tools failed. Check the output above for details.")
        sys.exit(1)


if __name__ == "__main__":
    main()